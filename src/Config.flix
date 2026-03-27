mod Config {
    use Fs.{FileWrite, FileRead}
    use Sys.Exit

    use Json.JsonElement
    use Json.FromJson
    use Json.ToJson
    use Json.JsonElement.{JsonString,JsonObject}
    use Json.JsonError
    use Json.JsonError.JsonError
    use Spotify.TrackObject

    use Config.Config

    pub enum Rule with ToString {
        case Follow(TrackObject, TrackObject)
    }

    pub enum Config({
        rules = List[Rule]
    })

    instance ToString[Config] {
        pub def toString(x: Config): String = {
            let Config({ rules }) = x;
            "Config({ rules = ${rules} })"
        }
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

    instance FromJson[Config] {
        pub def fromJsonAt(p: Json.Path.Path, x: JsonElement): Result[JsonError, Config] = {
            forM (
                map <- FromJson.fromJsonAt(p, x);
                rules <- Json.getAtKey(p, "rules", map) 
            ) yield {
                Config({ rules = rules })
            }
        } 
    }
    
    instance ToJson[Config] {
        pub def toJson(x: Config): JsonElement = {
            let Config({ rules }) = x;
            JsonObject(Map#{
                "rules" => Json.ToJson.toJson(rules)
            })
        }
    }
    
    pub def configToJsonString(config: Config): String = {
        Json.Write.toPrettyString(4, ToJson.toJson(config))
    }

    pub def writeConfigToFile(config: Config, filename: String): Result[IoError, Unit] \ {FileWrite} = {
        let contents = configToJsonString(config);
        FileWrite.write({ str = contents }, filename) 
    }
    
    pub def readConfigFromFile(filename: String): Result[String, Config] \ {FileRead} = {
        forM (
            contents <- FileRead.read(filename) |> Result.mapErr(_ -> "Could not read file");
            json <- Json.Parse.parse(contents) |> Option.toOk("Not valid JSON");
            config <- FromJson.fromJson(json) |> Result.mapErr(_ -> "JSON could not be parsed into Config")
        ) yield config
    }

    pub eff ConfigRead {
        def getConfig(): Config
    }

    pub def handleWithConfigFile(
        filename: String,
        f: a -> b \ ef
    ): a -> b \ (ef - ConfigRead) + FileRead + Exit = {
        x -> {
            // FIXME: it might be better to read anew each time
            match readConfigFromFile(filename) {
                case Ok(config) => {
                    run { f(x) } with handler ConfigRead {
                        def getConfig(_, resume) = resume(config)
                    }
                }
                case Err(_) => Exit.exit(1)
            }
        }
    }
    
    pub def runWithConfigFile(
        filename: String,
        f: Unit -> a \ ef
    ): a \ (ef - ConfigRead) + FileRead + Exit = handleWithConfigFile(filename, f)()

}