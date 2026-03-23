mod Config {
    use Sys.Exit
    use Config.Config
    use Fs.FileRead

    pub enum Config({
        spotifyApi = {
            clientId = String,
            clientSecret = String,
            refreshToken = String
        }
    })

    pub eff ConfigRead {
        def getConfig(): Config
    }

    def makeConfig(clientId: String, clientSecret: String,
                refreshToken: String): Config =
        Config({ spotifyApi = {
            clientId = clientId,
            clientSecret = clientSecret,
            refreshToken = refreshToken }
        })

    def parseEntry(line: String): Option[(String, String)] = {
        let (k, equals_v) = String.breakOnLeft({ substr = "=" }, line);
        forM (
            v <- String.stripPrefix({ substr = "=" }, equals_v)
        ) yield (k, v)
    }

    enum ValidationError with ToString {
        case MissingKey(String)
        case DuplicateKey(String)
    }

    enum ConfigReadError with ToString {
        case IoError(IoError)
        case ValidationErrors(Nec[ValidationError])
    }

    def validateConfigKey(key: String, raw: MultiMap[String, String]): Validation[ValidationError, String] = {
        match MultiMap.get(key, raw) |> Set.toList {
            case v :: Nil => Validation.Success(v)
            case Nil => Validation.Failure(Nec.singleton(ValidationError.MissingKey(key)))
            case _ => Validation.Failure(Nec.singleton(ValidationError.DuplicateKey(key)))
        }
    }

    def validateConfig(raw: MultiMap[String, String]): Validation[ValidationError, Config] = {
        forA (
            clientId <- validateConfigKey("SPOTIFY_CLIENT_ID", raw);
            clientSecret <- validateConfigKey("SPOTIFY_CLIENT_SECRET", raw);
            refreshToken <- validateConfigKey("SPOTIFY_REFRESH_TOKEN", raw)
        ) yield {
            makeConfig(clientId, clientSecret, refreshToken)
        }
    }

    def listToMultiMap(xs: List[(a, b)]): MultiMap[a, b] with Order[a], Order[b] = {
        List.foldLeft(
            (acc, pair) -> {
                let (k, v) = pair;
                MultiMap.insert(k, v, acc)
            },
            MultiMap.empty(),
            xs
        )
    }

    pub def readConfigFromFile(filename: String): Result[ConfigReadError, Config] \ FileRead = {
        forM (
            configLines <- FileRead.readLines(filename)
                |> Result.mapErr(ConfigReadError.IoError);
            config <- configLines
                |> List.filterMap(parseEntry)
                |> listToMultiMap
                |> validateConfig
                |> Validation.toResult
                |> Result.mapErr(ConfigReadError.ValidationErrors)
        ) yield {
            config
        }
    }

    pub def handleWithDotenv(f: a -> b \ ef): a -> b \ (ef - ConfigRead) + FileRead + Exit = {
        x -> {
                match readConfigFromFile(".env") {
                    case Ok(config) =>
                        run { f(x) } with handler ConfigRead {
                            def getConfig(_, resume) = resume(config)
                        }
                    case Err(_) => Exit.exit(1)
                }
            }
    }
    
    pub def runWithDotenv(f: Unit -> a \ ef): a \ (ef - ConfigRead) + FileRead + Exit = handleWithDotenv(f)()
}