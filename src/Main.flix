use Sys.Exit
use Net.Http
use Time.Clock
use Time.Sleep
use Fs.FileRead

def main(): Unit \ {Logger, Http, FileRead, Exit, Clock, Sleep} =
    run {
        let x = Config.ConfigRead.getConfig();
        Logger.debug(Config.configToJsonString(x));
        // Logger.debug(x);
        App.loop()
    }   
        with Http.withLogging
        with Config.runWithConfigFile("config.json")
        with Spotify.runWithRotatingAccessToken(Time.Duration.minutes(30))
        with Secrets.runWithDotenv