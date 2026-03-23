use Sys.Exit.exit
use Net.Http
use Sys.Console

def main(): Unit \ {IO, Logger, Http, FileReadWithResult} =
    run {
        match readConfigFromFile(".env") {
            case Ok(config) =>
                run {
                    let q = forM (
                            accessToken <- Spotify.getAccessToken();
                            queue <- Spotify.getQueue(accessToken)
                        ) yield queue;
                    match q {
                        case Ok(queue) => Console.println(ToString.toString(queue))
                        case Err(e) => Console.println(e)
                    }
                } with handler ConfigRead {
                    def getConfig(_, resume) = 
                        resume(config)
                }
            case Err(e) => Console.println(ToString.toString(e))
        }
    } with Console.runWithIO