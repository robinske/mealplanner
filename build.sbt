name := "pinterest-meal-planner"

scalaVersion := "2.11.7"

version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalaj"         %% "scalaj-http"    % "2.2.1",
  "com.github.finagle" %% "finch-core"     % "0.10.0",
  "com.twitter"        %% "twitter-server" % "1.18.0",
  "io.argonaut"        %% "argonaut"       % "6.0.4"
)

resolvers += "twttr" at "https://maven.twttr.com/"

cancelable in Global := true
