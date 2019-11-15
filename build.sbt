lazy val root = project
  .in(file("."))
  .settings(
    name := "scala19-gadt-code",
    description := "Dotty code accompanying Scala'19 paper on GADTs",
    version := "0.1.0",

    scalaVersion := "0.17.0-RC1"
  )
