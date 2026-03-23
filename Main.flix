use Sys.Exit
use Net.Http
use Sys.Console
use Time.Clock
use Fs.FileRead

def main(): Unit \ {Console, Logger, Http, FileRead, Exit, Clock} =
    run {
        let x = forM (
            queue1 <- Spotify.getQueue();
            _ <- Spotify.addToQueue();
            queue2 <- Spotify.getQueue()
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
        with Spotify.runWithRotatingAccessToken(Time.Duration.minutes(30))
        with Secrets.runWithDotenv