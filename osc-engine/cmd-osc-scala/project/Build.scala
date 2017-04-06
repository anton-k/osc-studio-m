import sbt._

object MyBuild extends Build {

  lazy val root = Project("root", file("."))
        .dependsOn(oscProject)        
  
  lazy val oscProject = RootProject(uri("git://github.com/anton-k/scala-simple-osc.git#master"))
}
