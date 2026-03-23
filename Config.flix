mod Config {
    use Json.JsonElement.{JsonString,JsonObject}
    use Json.ToJson
    use Json.JsonElement
    use Json.JsonError
    use Json.JsonError.JsonError
    use Json.FromJson
    use Spotify.TrackObject

    enum Rule {
        case Follow(TrackObject, TrackObject)
    }

    def ruleFromJson(p: Json.Path.Path, map: Map[String, JsonElement], ruleType: String): Result[JsonError, Rule] = {
        match ruleType {
            case "follow" => {
                forM (
                    currTrack <- Json.getAtKey(p, "current", map);
                    queueTrack <- Json.getAtKey(p, "queue", map)
                ) yield {
                    Rule.Follow(currTrack, queueTrack)
                }
            }
            case _ => Err(JsonError(p, Set.singleton("unknown rule type")))
        }
    }

    instance FromJson[Rule] {
        pub def fromJsonAt(p: Json.Path.Path, x: Json.JsonElement): Result[JsonError, Rule] = {
            forM (
                map <- FromJson.fromJsonAt(p, x);
                ruleType <- Json.getAtKey(p, "type", map);
                rule <- ruleFromJson(p, map, ruleType)
            ) yield rule
        }
    }
    
    instance ToJson[Rule] {
        pub def toJson(x: Rule): JsonElement = {
            match x {
                case Rule.Follow(a, b) => {
                    JsonObject(Map#{
                        "current" => Json.ToJson.toJson(a),
                        "queue" => Json.ToJson.toJson(b)
                    })
                }
            }
        }
    }
}