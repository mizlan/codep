mod App {
    use Config.readConfigFromFile
    use Time.Sleep
    use Spotify.QueueResponseObject.QueueResponseObject

    pub def loop(): Void \ {Logger, Sleep, Net.Http, Spotify.AccessTokenRead} = {
        match Spotify.getQueue() {
            case Ok(QueueResponseObject(v)) => Logger.info(v#currentlyPlaying)
            case Err(e) => Logger.warn(e)
        };
        Sleep.sleep(Time.Duration.seconds(5));
        loop()
    }
}