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
                currentlyPlaying <- Json.getAtKeyOpt(p, "currently_playing", map);
                queue <- Json.getAtKey(p, "queue", map)
            ) yield {
                QueueResponseObject({ currentlyPlaying = currentlyPlaying, queue = queue })
            }
        }
    }
    
    def parseQueueFromResponse(resp: HttpResponse): Result[String, QueueResponseObject] = {
        let jsonElement = resp |> HttpResponse.body |> Json.Parse.parse;
        match jsonElement {
            case None => Err("invalid json")
            case Some(json) => {
                match Json.FromJson.fromJson(json) {
                    case Ok(v) => Ok(v)
                    case Err(_) => Err("json error")
                }
            }
        }
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
    
    pub def getAccessToken(): Result[String, AccessToken] \ {Http, Logger, ConfigRead} = {
        // taken from https://developer.spotify.com/documentation/web-api/tutorials/getting-started
        let (Config.Config(config)) = ConfigRead.getConfig();
        let id = config#spotifyApi#clientId;
        let secret = config#spotifyApi#clientSecret;
        let body = "grant_type=client_credentials&client_id=${id}&client_secret=${secret}";
        let req =
            HttpRequest.post("https://accounts.spotify.com/api/token", body)
            |> HttpRequest.withContentType("application/x-www-form-urlencoded");
        run {
            match Http.send(req) {
                case Ok(resp) =>
                    parseAccessTokenFromResponse(resp)
                case Err(err) =>
                    Logger.warn("Failed to get token ${err}");
                    Err("cooked")
            }
        } with Http.withLogging
    }

    pub def getQueue(accessToken: AccessToken): Result[String, QueueResponseObject] \ {Http, Logger} = {
        let req = HttpRequest.get("https://api.spotify.com/v1/me/player/queue")
            |> HttpRequest.withBearerToken(coerce(accessToken));
        run {
            forM (
                resp <- Http.send(req) |> Result.mapErr(_ -> "HTTP error");
                queue <- parseQueueFromResponse(resp)
            ) yield queue
        } with Http.withLogging
    }
}
