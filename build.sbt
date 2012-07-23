name := "token-bucket"

version := "0.1"

scalaVersion := "2.9.2"

libraryDependencies ++= {
  val t = "test"
  Seq(
	"org.specs2" %% "specs2" % "1.10" % t,
	"org.specs2" %% "specs2-scalaz-core" % "6.0.1" % t,
	"junit" % "junit" % "4.7" % t,
	"org.hamcrest" % "hamcrest-all" % "1.1" % t,
	"org.scala-tools.testing" % "test-interface" % "0.5" % t,
	"org.scalacheck" %% "scalacheck" % "1.9" % t
  )
}
