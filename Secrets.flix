mod Secrets {
    use Sys.Exit
    use Secrets.Secrets
    use Fs.FileRead

    pub enum Secrets({
        spotifyApi = {
            clientId = String,
            clientSecret = String,
            refreshToken = String
        }
    })

    pub eff SecretsRead {
        def getSecrets(): Secrets
    }

    def makeSecrets(clientId: String, clientSecret: String,
                refreshToken: String): Secrets =
        Secrets({ spotifyApi = {
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

    enum SecretsReadError with ToString {
        case IoError(IoError)
        case ValidationErrors(Nec[ValidationError])
    }

    def validateSecretsKey(key: String, raw: MultiMap[String, String]): Validation[ValidationError, String] = {
        match MultiMap.get(key, raw) |> Set.toList {
            case v :: Nil => Validation.Success(v)
            case Nil => Validation.Failure(Nec.singleton(ValidationError.MissingKey(key)))
            case _ => Validation.Failure(Nec.singleton(ValidationError.DuplicateKey(key)))
        }
    }

    def validateSecrets(raw: MultiMap[String, String]): Validation[ValidationError, Secrets] = {
        forA (
            clientId <- validateSecretsKey("SPOTIFY_CLIENT_ID", raw);
            clientSecret <- validateSecretsKey("SPOTIFY_CLIENT_SECRET", raw);
            refreshToken <- validateSecretsKey("SPOTIFY_REFRESH_TOKEN", raw)
        ) yield {
            makeSecrets(clientId, clientSecret, refreshToken)
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

    def readSecretsFromFile(filename: String): Result[SecretsReadError, Secrets] \ FileRead = {
        forM (
            secretsLines <- FileRead.readLines(filename)
                |> Result.mapErr(SecretsReadError.IoError);
            secrets <- secretsLines
                |> List.filterMap(parseEntry)
                |> listToMultiMap
                |> validateSecrets
                |> Validation.toResult
                |> Result.mapErr(SecretsReadError.ValidationErrors)
        ) yield {
            secrets
        }
    }

    pub def handleWithSecretsFile(filename: String, f: a -> b \ ef): a -> b \ (ef - SecretsRead) + FileRead + Exit = {
        x -> {
            match readSecretsFromFile(filename) {
                case Ok(secrets) =>
                    run { f(x) } with handler SecretsRead {
                        def getSecrets(_, resume) = resume(secrets)
                    }
                case Err(_) => Exit.exit(1)
            }
        }
    }

    pub def runWithSecretsFile(filename: String, f: Unit -> a \ ef): a \ (ef - SecretsRead) + FileRead + Exit = handleWithSecretsFile(filename, f)()

    pub def handleWithDotenv(f: a -> b \ ef): a -> b \ (ef - SecretsRead) + FileRead + Exit =
        handleWithSecretsFile(".env", f)

    pub def runWithDotenv(f: Unit -> a \ ef): a \ (ef - SecretsRead) + FileRead + Exit = handleWithDotenv(f)()
}