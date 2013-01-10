name := "Floyd.main"

version := "3.0"

scalaVersion := "2.9.2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
    "me.prettyprint" % "hector-core" % "1.0-1",
    "junit" % "junit" % "4.5" % "test",
    "com.novocode" % "junit-interface" % "0.8" % "test",
    "org.apache.camel" % "camel-core" % "2.8.3",
    "org.apache.camel" % "camel-mail" % "2.8.3",
    "org.apache.httpcomponents" % "httpclient" % "4.1.2",
    "org.scalacheck" %% "scalacheck" % "1.9" % "test",
    "org.scalatest" %% "scalatest" % "1.8" % "test",
    "com.typesafe.akka" % "akka-actor" % "2.0.2",
    "com.typesafe.akka" % "akka-remote" % "2.0.2",
    "org.quartz-scheduler" % "quartz" % "2.1.5",
    "org.quartz-scheduler" % "quartz-jboss" % "2.1.5",
    "log4j" % "log4j" % "1.2.14"
)

