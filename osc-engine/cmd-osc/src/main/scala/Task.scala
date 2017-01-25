import scala.collection.mutable
import sys.process._

case class TaskManager {
    private var tasks = mutable.HashMap[String,Task]()

    def run(name: String, cmd: String) {
        tasks.get(name).foreach { t => t.destroy }
        tasks += (name -> Task(cmd))
    }

    def restart(name: String) {
        tasks.get(name).foreach { t =>
            t.destroy            
            tasks += (name -> Task(t.cmd))
        }
    }

    def kill(name: String) {
        tasks.get(name).foreach { t =>
            t.destroy
            tasks -= name
        }
    }
}

case class Task(cmd: String) {
    println("run: " + cmd)
    val process = cmd.run

    def destroy {
        process.destroy
    }    
}