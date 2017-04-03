mainClass in (Compile,run) := Some("App")

libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.5.0" )   

lazy val commonSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "com.example",
  scalaVersion := "2.10.1",
  test in assembly := {}
)

lazy val app = (project in file("app")).
  settings(commonSettings: _*).
  settings(
    mainClass in assembly := Some("App")
    // more settings here ...
  )

assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}
