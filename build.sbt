

scalaVersion := "2.11.7"
val generatedDir = file("generated")
lazy val root = Project("demo", file(".")).enablePlugins(ScalaJSPlugin)

libraryDependencies ++=
  Seq(
    "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.1.2",
    "org.scala-js" %%% "scalajs-dom" % "0.8.1",
    "be.doeraene" %%% "scalajs-jquery" % "0.8.1",
    "org.monifu" %%% "monifu" % "1.0-RC3",
    "com.lihaoyi" %%% "upickle" % "0.3.1") ++
    Seq("core", "extra")
      .map("com.github.japgolly.scalajs-react" %%% _ % "0.9.2") ++
    Seq("nyaya-core", "nyaya-test")
      .map("com.github.japgolly.nyaya" %%% _ % "0.5.11" % "test")

jsDependencies ++= Seq(
  RuntimeDOM,
  "org.webjars" % "react" % "0.12.2" / "react-with-addons.js" commonJSName "React" minified "react-with-addons.min.js")

testFrameworks += new TestFramework("utest.runner.Framework")

skip in packageJSDependencies := false
scalaJSStage in Global := FastOptStage

workbenchSettings
bootSnippet := "presentation.Demo().main();"
refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile)

/* move these files out of target/. Also sets up same file for both fast and full optimization */
crossTarget  in (Compile, fullOptJS)                     := generatedDir
crossTarget  in (Compile, fastOptJS)                     := generatedDir
crossTarget  in (Compile, packageJSDependencies)         := generatedDir
crossTarget  in (Compile, packageScalaJSLauncher)        := generatedDir
crossTarget  in (Compile, packageMinifiedJSDependencies) := generatedDir
artifactPath in (Compile, fastOptJS)                     :=
  ((crossTarget in (Compile, fastOptJS)).value / ((moduleName in fastOptJS).value + "-opt.js"))
