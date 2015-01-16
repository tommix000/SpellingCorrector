name := "SpellingCorrector"

version := "1.0"

scalaVersion := "2.10.4"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-optimise",
  "-explaintypes",
  "-Xcheckinit",
  "-Xfatal-warnings",
  "-Xlint",
  "-Xverify",
  "-Yclosure-elim",
  "-Ydead-code",
  "-Yinline",
  "-Ywarn-all",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps"
)

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
    