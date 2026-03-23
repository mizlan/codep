use Sys.Exit
use Net.Http
use Sys.Console
use Time.Clock
use Time.Sleep
use Fs.FileRead

def main(): Unit \ {Logger, Http, FileRead, Exit, Clock, Sleep} =
    run {
        App.loop()
    }
        with Http.withLogging
        with Spotify.runWithRotatingAccessToken(Time.Duration.minutes(30))
        with Secrets.runWithDotenv