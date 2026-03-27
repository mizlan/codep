mod Spotify {
    use Json.ToJson
    use Json.FromJson
    use Json.JsonElement
    use Json.JsonElement.{JsonString,JsonObject}
    use Json.JsonError
    use Json.JsonElement.JsonObject
    use Json.JsonElement.JsonString

    use Net.Http
    use Net.HttpResponse
    use Net.HttpRequest

    use TrackObject.TrackObject
    use QueueResponseObject.QueueResponseObject
    use Secrets.Secrets.Secrets
    use Secrets.SecretsRead

    enum AccessToken(String) with ToString, Coerce

    pub enum TrackObject({
        name = String,
        uri = String
    })

    instance Eq[TrackObject] {
        pub def eq(x: TrackObject, y: TrackObject): Bool = {
            match (x, y) {
                case (
                    TrackObject({ name = name1, uri = uri1}),
                    TrackObject({ name = name2, uri = uri2 })
                ) => {
                    (name1 == name2) and (uri1 == uri2)
                }
            }
        }
    }

    instance ToString[TrackObject] {
        pub def toString(track: TrackObject): String = {
            let (TrackObject(x)) = track;
            "TrackObject({ name = ${x#name}, uri = ${x#uri} })"
        }
    }
    
    instance Formattable[TrackObject] {
        pub def format(x: TrackObject): RichString = RichString.fromString(ToString.toString(x))
    }
    
    pub enum QueueResponseObject({
        currentlyPlaying = Option[TrackObject],
        queue = List[TrackObject]
    })

    instance ToString[QueueResponseObject] {
        pub def toString(queueResp: QueueResponseObject): String = {
            let (QueueResponseObject(x)) = queueResp;
            "QueueResponseObject({ currentlyPlaying = ${x#currentlyPlaying}, queue = ${x#queue} })"
        }
    }

    instance Formattable[QueueResponseObject] {
        pub def format(x: QueueResponseObject): RichString = RichString.fromString(ToString.toString(x))
    }

    instance FromJson[TrackObject] {
        pub def fromJsonAt(p: Json.Path.Path, x: JsonElement): Result[JsonError, TrackObject] = {
            forM (
                map <- FromJson.fromJsonAt(p, x);
                name <- Json.getAtKey(p, "name", map);
                uri <- Json.getAtKey(p, "uri", map)
            ) yield {
                TrackObject({ name = name, uri = uri })
            }
        }
    }

    instance ToJson[TrackObject] {
        pub def toJson(x: TrackObject): JsonElement = {
            match x {
                case (TrackObject({ name, uri })) => {
                    JsonObject(Map#{
                        "name" => JsonString(name),
                        "uri" => JsonString(uri)
                    })
                }
            }
        }
    }

    instance FromJson[QueueResponseObject] {
        pub def fromJsonAt(p: Json.Path.Path, x: JsonElement): Result[JsonError, QueueResponseObject] = {
            forM (
                map <- FromJson.fromJsonAt(p, x);
                currentlyPlayingOpt <- Json.getAtKey(p, "currently_playing", map);
                currentlyPlaying <- FromJson.fromNullableJsonAt(p, currentlyPlayingOpt);
                queue <- Json.getAtKey(p, "queue", map)
            ) yield {
                QueueResponseObject({ currentlyPlaying = currentlyPlaying, queue = queue })
            }
        }
    }
    
    def parseQueueFromResponse(resp: HttpResponse): Result[String, QueueResponseObject] = {
        forM (
            json <- resp
                |> HttpResponse.body
                |> Json.Parse.parse
                |> Option.toOk("Invalid JSON");
            queue <- Json.FromJson.fromJson(json)
                |> Result.mapErr(_ -> "Queue parsing error")
        ) yield queue
    }

    def parseAccessTokenFromResponse(resp: HttpResponse): Result[String, AccessToken] = {
        let respBody = HttpResponse.body(resp);
        match Json.Parse.parse(respBody) {
            case Some(JsonObject(map)) => {
                match Map.get("access_token", map) {
                    case Some(JsonString(s)) => Ok(AccessToken.AccessToken(s))
                    case _ => Err("no access token string field")
                }
            }
            case _ => Err("json invalid")
        }
    }


    pub def generateAccessToken(): Result[String, AccessToken] \ {Http, SecretsRead} = {  
        let req = {
            let Secrets(secrets) = SecretsRead.getSecrets();

            let basicAuth = {
                let id = secrets#spotifyApi#clientId;
                let secret = secrets#spotifyApi#clientSecret;
                Base64.encode("${id}:${secret}")
            };
            let refresh = secrets#spotifyApi#refreshToken;
            let body = "grant_type=refresh_token&refresh_token=${refresh}&scope=user-read-playback-state%20user-modify-playback-state";

            HttpRequest.post("https://accounts.spotify.com/api/token", body)
            |> HttpRequest.withContentType("application/x-www-form-urlencoded")
            |> Net.HttpRequest.withHeader("Authorization", "Basic ${basicAuth}")
        };
        match Http.send(req) {
            case Ok(resp) =>
                parseAccessTokenFromResponse(resp)
            case Err(_) =>
                Err("did not get token")
        }
    }

    
    pub eff AccessTokenRead {
        def getAccessToken(): Result[String, AccessToken]
    }

    pub def handleWithRotatingAccessToken(
        expiresAfter: Time.Duration,
        f: a -> b \ ef
    ): a -> b \ ef - AccessTokenRead + Http + SecretsRead + Time.Clock = {
        x -> region rc {
            let lastToken = Ref.fresh(rc, None);
            def resumeWithGenerated(resume) = {
                match generateAccessToken() {
                    case Ok(accessToken) => {
                        let now = Time.Clock.now();
                        Ref.put(Some((accessToken, now)), lastToken);
                        resume(Ok(accessToken))
                    }
                    case Err(e) => resume(Err(e))
                }
            };
            run { f(x) } with handler AccessTokenRead {
                def getAccessToken(_, resume) = {
                    match Ref.get(lastToken) {
                        case None => {
                            resumeWithGenerated(resume)
                        }
                        case Some((accessToken, lastRefreshTime)) => {
                            let now = Time.Clock.now();
                            if (now > lastRefreshTime + Time.Duration.toMillis(expiresAfter)) {
                                Ref.put(None, lastToken);
                                resumeWithGenerated(resume)
                            } else {
                                resume(Ok(accessToken))
                            }
                        }
                    }
                }
            }
        }
    }

    pub def runWithRotatingAccessToken(
        expiresAfter: Time.Duration,
        f: Unit -> a \ ef
    ): a \ ef - AccessTokenRead + Http + Secrets.SecretsRead + Time.Clock = {
        handleWithRotatingAccessToken(expiresAfter, f)()
    }

    def httpSendWithAccessToken(accessToken: AccessToken, req: HttpRequest): Result[IoError, HttpResponse] \ {Http} = {
        let accessTokenStr: String = coerce(accessToken);
        req
        |> HttpRequest.withHeader("Authorization", "Bearer ${accessTokenStr}")
        |> Http.send
    }

    pub def getQueue(): Result[String, QueueResponseObject] \ {Http, AccessTokenRead} = {
        forM (
            accessToken <- AccessTokenRead.getAccessToken();
            resp <- HttpRequest.get("https://api.spotify.com/v1/me/player/queue")
                |> httpSendWithAccessToken(accessToken)
                |> Result.mapErr(_ -> "HTTP error");
            queue <- parseQueueFromResponse(resp)
        ) yield queue
    }

    pub def addToQueue(track: TrackObject): Result[String, Unit] \ {Http, AccessTokenRead} = {
        let TrackObject({ uri | _ }) = track;
        let queryParams = "uri=${uri}";
        forM (
            accessToken <- AccessTokenRead.getAccessToken();
            resp <- HttpRequest.post("https://api.spotify.com/v1/me/player/queue?${queryParams}", "")
                |> httpSendWithAccessToken(accessToken)
                |> Result.mapErr(_ -> "HTTP error");
            res <- if (HttpResponse.isSuccess(resp)) {
                    Ok(())
                } else {
                    Err("Failed")
                }
        ) yield {
            res
        }
    }
}
