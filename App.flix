mod App {
    use Config.ConfigRead
    use Config.Rule
    use Config.Config
    use Config.Config.Config
    use Time.Sleep
    use Spotify.QueueResponseObject
    use Spotify.QueueResponseObject.QueueResponseObject
    use Spotify.TrackObject.TrackObject

    def findMatchingRule(config: Config, queue: QueueResponseObject): Option[Rule] = {
        let Config({ rules }) = config;
        List.find(rule -> ruleMatchesQueue(rule, queue), rules)
    }

    def ruleMatchesQueue(rule: Rule, queue: QueueResponseObject): Bool = {
        match rule {
            case Rule.Follow(TrackObject(a), TrackObject(b)) => {
                match queue {
                    case QueueResponseObject({
                        currentlyPlaying = Some(TrackObject(track1)),
                        queue = TrackObject(track2) :: _
                    }) if track1#uri == a#uri and track2#uri != b#uri => true 
                    case QueueResponseObject({
                        currentlyPlaying = Some(TrackObject(track1)),
                        queue = Nil
                    }) if track1#uri == a#uri => true 
                    case _ => false
                }
            }
        }
    }
    
    def applyRule(rule: Rule): Result[String, Unit] \ {} = {
        unreachable!()
    }

    @Tailrec
    pub def loop(): Void \ {Logger, Sleep, Net.Http, Spotify.AccessTokenRead, ConfigRead} = {
        match Spotify.getQueue() {
            // case Ok(QueueResponseObject(v)) => Logger.info(v#currentlyPlaying)
            case Ok(queue) => {
                match findMatchingRule(ConfigRead.getConfig(), queue) {
                    case Some(rule) => {
                        match applyRule(rule) {
                            case Ok(()) => Logger.info("Applied rule")
                            case Err(e) => Logger.warn(e)
                        }
                    }
                    case None => ()
                }
            }
            case Err(e) => Logger.warn(e)
        };
        Sleep.sleep(Time.Duration.seconds(5));
        loop()
    }
}