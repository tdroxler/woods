addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.1.0" cross CrossVersion.full)
scalacOptions += "-Yrangepos"

lazy val module = (project in file("module"))

lazy val root = (project in file("."))
  .aggregate(module)
  .dependsOn(module)
