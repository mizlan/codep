use Sys.Exit
use Net.Http
use Sys.Console

def main(): Unit \ {Console, Logger, Http, FileReadWithResult, Exit} =
    run {
        let x = forM (
            accessToken <- Spotify.generateAccessToken();
            queue1 <- Spotify.getQueue(accessToken);
            _ <- Spotify.addToQueue(accessToken);
            queue2 <- Spotify.getQueue(accessToken)
        ) yield {
            Console.println(ToString.toString(queue1));
            Console.println(ToString.toString(queue2))
        };
        match x {
            case Ok() => ()
            case Err(e) => Console.println(e)
        }
    }
        with Http.withLogging
        with Config.runWithDotenv