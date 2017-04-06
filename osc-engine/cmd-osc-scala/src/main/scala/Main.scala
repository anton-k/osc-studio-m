
object App {
    def main(rawArgs: Array[String]) {
        ReadArgs(rawArgs) match {
            case Some(args) => {
                val s = Server(args)
                waitForever(s)
            }
            case None => {
                println("Please use --help argument for usage")
            }
        }        
    }

    def waitForever(server: Server) {
        var flag = true
        while (flag) {
            Thread.sleep(5)
            flag = server.isRunning
        }
    }
}
