name := """HybridParser"""

organization := "org.ababup1192"

version := "0.2.5"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "name.lakhin.eliah.projects.papacarlo" % "papa-carlo_2.11" % "0.8.0-SNAPSHOT",
  "io.spray" %%  "spray-json" % "1.3.2"
)



// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

