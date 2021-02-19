name := "learnAlgs"
version := "0.1"
//scalaVersion := "2.13.4"
scalaVersion := "3.0.0-RC1"
//javacOptions := Seq("-target", "1.8")
fork in run := true

// https://mvnrepository.com/artifact/org.scalatest/scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % Test
// https://mvnrepository.com/artifact/org.slf4j/slf4j-api
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25"
// https://mvnrepository.com/artifact/ch.qos.logback/logback-core
libraryDependencies += "ch.qos.logback" % "logback-core" % "1.2.3"
// https://mvnrepository.com/artifact/ch.qos.logback/logback-classic
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
// https://mvnrepository.com/artifact/com.typesafe.play/play-json
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.8.1"
//libraryDependencies += "com.mazhangjing" % "algorithm" % "1.0.0"
lazy val javaFXVersion = "15.0.1"
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}
lazy val javaFXModules = Seq("base", "controls", "graphics")
libraryDependencies ++= javaFXModules.map( m =>
  "org.openjfx" % s"javafx-$m" % javaFXVersion classifier osName
)
libraryDependencies += "org.scalafx" %% "scalafx" % s"$javaFXVersion-R21"
libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value))
//scalacOptions ++= { if (isDotty.value) Seq("-source:3.0-migration","-rewrite") else Nil }