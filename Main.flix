use Net.Http
use Sys.Console

def main(): Unit \ {Console, Logger, Http, FileReadWithResult} =
    let result = forM (
        config <- readConfigFromFile(".env") |> Result.mapErr(ToString.toString);
        queue  <- run {
                    forM (
                        accessToken <- Spotify.getAccessToken();
                        queue <- Spotify.getQueue(accessToken)
                    ) yield queue
                } with handler ConfigRead {
                    def getConfig(_, resume) = resume(config)
                } with Http.withLogging
    ) yield queue;
    match result {
        case Ok(queue) => Console.println(ToString.toString(queue))
        case Err(e)    => Console.println(e)
    }