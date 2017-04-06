// command line proces launcher with OSC
//
// OSC API
//
//   /run  string                   -- runs a command 
//   /kill name                     -- kill all processes
//   /exit                          -- stop self
// 
//   /process/run      name cmd     -- starts named process
//   /process/restart  name         -- restarts process by name
//   /proces/kill      name         -- kills process by name
import scala.audio.osc._

case class Server(args: Args) {
    private val server = OscServer(args.port)
    private var alive = true

    val filter = (args.whiteList, args.blackList) match {
        case (None, None) => (x: String) => true
        case (Some(whites), None) => onWhite(whites) _
        case (None, Some(blacks)) => onBlack(blacks) _
        case (Some(whites), Some(blacks)) => { 
            val wh = onWhite(whites) _
            val bl = onBlack(blacks) _
            (x: String) => wh(x) && bl(x)
        }
    }

    def withFilter(cmd: String)(proc: => Unit) {
        if (filter(cmd)) {
            proc
        } else {
            println("Execution is not permited.")
        }
    }

    private var mgr = TaskManager()  
    private var mainProc: Option[Task] = None

    server.listen[String]("/run") { cmd => withFilter(cmd) {
        mainProc.foreach( t => t.destroy )
        mainProc = Some(Task(cmd))
    }}

    server.listen[Unit]("/kill") { _ => mainProc.foreach( t => t.destroy ) }

    server.listen[Unit]("/exit") { _ => 
        println("exit")       
        alive = false
        server.close 
    }

    server.listen[(String, String)]("/process/run") { case (name, cmd) => withFilter(cmd) {
        println(cmd)
        mgr.run(name, cmd)
    }}

    server.listen[String]("/process/restart") { name => mgr.restart(name) }
    server.listen[String]("/process/kill")    { name => mgr.kill(name) }

    def isRunning = alive

    def onWhite(prefs: List[String])(str: String) = inList(prefs)(str)
    def onBlack(prefs: List[String])(str: String) = !inList(prefs)(str)

    def inList(prefs: List[String])(str: String)  = 
        prefs.exists(x => str.startsWith(x))
}
