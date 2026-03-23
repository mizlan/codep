mod Spotify {
    use Json.FromJson
    use Json.JsonElement
    use Json.JsonError
    use Json.JsonElement.JsonObject
    use Json.JsonElement.JsonString

    use Net.Http
    use Net.HttpResponse
    use Net.HttpRequest

    use TrackObject.TrackObject
    use QueueResponseObject.QueueResponseObject
    use Config.Config.Config

    enum AccessToken(String) with ToString, Coerce

    enum TrackObject({
        name = String,
        uri = String
    })

    instance ToString[TrackObject] {
        pub def toString(track: TrackObject): String = {
            let (TrackObject(x)) = track;
            "TrackObject({ name = ${x#name}, uri = ${x#uri} })"
        }
    }
    
    enum QueueResponseObject({
        currentlyPlaying = Option[TrackObject],
        queue = List[TrackObject]
    })

    instance ToString[QueueResponseObject] {
        pub def toString(queueResp: QueueResponseObject): String = {
            let (QueueResponseObject(x)) = queueResp;
            "QueueResponseObject({ currentlyPlaying = ${x#currentlyPlaying}, queue = ${x#queue} })"
        }
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
        Debug.dprintln(resp |> HttpResponse.body);
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
    
    // eff AccessTokenRead {
    //     def getAccessToken(): AccessToken
    // }

    pub def generateAccessToken(): Result[String, AccessToken] \ {Http, Config.ConfigRead} = {  
        let req = {
            let Config(config) = Config.ConfigRead.getConfig();

            let basicAuth = {
                let id = config#spotifyApi#clientId;
                let secret = config#spotifyApi#clientSecret;
                Base64.encode("${id}:${secret}")
            };
            let refresh = config#spotifyApi#refreshToken;
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

    pub def getQueue(accessToken: AccessToken): Result[String, QueueResponseObject] \ {Http} = {
        let accessTokenStr: String = coerce(accessToken);
        let req = HttpRequest.get("https://api.spotify.com/v1/me/player/queue")
            |> HttpRequest.withHeader("Authorization", "Bearer ${accessTokenStr}");
        forM (
            resp <- Http.send(req) |> Result.mapErr(_ -> "HTTP error");
            queue <- parseQueueFromResponse(resp)
        ) yield queue
    }

    pub def addToQueue(accessToken: AccessToken): Result[String, Unit] \ {Http} = {
        let accessTokenStr: String = coerce(accessToken);
        let uri = "spotify:track:2tWioTW2rs0CXf1dTXlRrX";
        let queryParams = "uri=${uri}";
        let req = HttpRequest.post("https://api.spotify.com/v1/me/player/queue?${queryParams}", "")
            |> HttpRequest.withContentType("application/x-www-form-urlencoded")
            |> HttpRequest.withHeader("Authorization", "Bearer ${accessTokenStr}");
        forM (
            resp <- Http.send(req) |> Result.mapErr(_ -> "HTTP error");
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
