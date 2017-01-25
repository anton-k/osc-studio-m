import scopt.OptionParser

case class Args(
    port: Int = 5500, 
    whiteList: Option[List[String]] = None, 
    blackList: Option[List[String]] = None)

object ReadArgs {
    def apply(args: Array[String]) = 
        parser.parse(args, Args())
    
    val parser = new scopt.OptionParser[Args]("cmd-osc") {
        head("cmd-osc. Command line executor. You can send the messages on OSC-path /run with command as a single string argument. To use we have to specify a port <int> to listen for OSC-messages.", "0.1")       

        opt[Int]('p',"port").required().action(  (x, c) =>
            c.copy(port = x)).text("port is OSC port for server (the app listens on this port).")

        opt[Seq[String]]('w', "white-list").action { (x, c) =>
            c.copy(whiteList = Some(x.toList))
        }.text("command white list. Only commands that start with one of the prefixes is going to be executed.")

        opt[Seq[String]]('b', "black-list").action { (x, c) =>
            c.copy(whiteList = Some(x.toList))
        }.text("command black list. The Commands that start with one of the prefixes are not going to be executed.")

        help("help").text("Shows help")        
    }
}