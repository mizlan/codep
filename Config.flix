use Config.Config

pub enum Config({
    spotifyApi = {
        clientId = String,
        clientSecret = String
    }
})

eff ConfigRead {
    def getConfig(): Config
}

def makeConfig(clientId: String, clientSecret: String): Config =
    Config({ spotifyApi = { clientId = clientId, clientSecret = clientSecret }})

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
        clientSecret <- validateConfigKey("SPOTIFY_CLIENT_SECRET", raw)
    ) yield {
        makeConfig(clientId, clientSecret)
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

def readConfigFromFile(filename: String): Result[ConfigReadError, Config] \ FileReadWithResult = {
    forM (
        configLines <- FileReadWithResult.readLines(filename)
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