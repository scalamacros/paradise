import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import com.typesafe.sbt.pgp.PgpKeys._

object build extends Build {
  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.10.3",
    crossVersion := CrossVersion.full,
    version := "2.0.0-SNAPSHOT",
    organization := "org.scalamacros",
    description := "Empowers production Scala compiler with latest macro developments",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq("-deprecation", "-feature", "-optimise"),
    parallelExecution in Test := false, // hello, reflection sync!!
    logBuffered := false,
    scalaHome := {
      val scalaHome = System.getProperty("paradise.scala.home")
      if (scalaHome != null) {
        println(s"Going for custom scala home at $scalaHome")
        Some(file(scalaHome))
      } else None
    },
    sources in (Compile, doc) ~= (_ filter (_.getName endsWith ".scala")),
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>https://github.com/scalamacros/paradise</url>
      <inceptionYear>2012</inceptionYear>
      <licenses>
        <license>
          <name>BSD-like</name>
          <url>http://www.scala-lang.org/downloads/license.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/scalamacros/paradise.git</url>
        <connection>scm:git:git://github.com/scalamacros/paradise.git</connection>
      </scm>
      <issueManagement>
        <system>GitHub</system>
        <url>https://github.com/scalamacros/paradise/issues</url>
      </issueManagement>
      <developers>
        <developer>
          <id>xeno-by</id>
          <name>Eugene Burmako</name>
          <url>http://xeno.by</url>
        </developer>
      </developers>
    )
  )

  lazy val publishableSettings = sharedSettings ++ Seq(
    publishArtifact in Compile := true,
    publishArtifact in Test := false,
    credentials ++= {
      val mavenSettingsFile = System.getProperty("maven.settings.file")
      if (mavenSettingsFile != null) {
        println("Loading Sonatype credentials from " + mavenSettingsFile)
        try {
          import scala.xml._
          val settings = XML.loadFile(mavenSettingsFile)
          def readServerConfig(key: String) = (settings \\ "settings" \\ "servers" \\ "server" \\ key).head.text
          List(Credentials(
            "Sonatype Nexus Repository Manager",
            "oss.sonatype.org",
            readServerConfig("username"),
            readServerConfig("password")
          ))
        } catch {
          case ex: Exception =>
            println("Failed to load Maven settings from " + mavenSettingsFile + ": " + ex)
            Nil
        }
      } else {
        // println("Sonatype credentials cannot be loaded: -Dmaven.settings.file is not specified.")
        Nil
      }
    }
  )

  lazy val root = Project(
    id = "root",
    base = file("root")
  ) settings (
    sharedSettings : _*
  ) settings (
    test in Test := (test in tests in Test).value,
    packagedArtifacts := Map.empty
  ) aggregate (quasiquotes, plugin)

  lazy val myma = Project(
    id   = "myma",
    base = file("myma")
  ) settings (
    sharedSettings : _*
  ) settings (
    resourceDirectory in Compile <<= baseDirectory(_ / "src" / "main" / "scala" / "org" / "scalamacros" / "myma" / "embedded"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "provided")
  )

  lazy val quasiquotes = Project(
    id   = "quasiquotes",
    base = file("quasiquotes")
  ) settings (
    publishableSettings : _*
  ) settings (
    crossVersion := CrossVersion.binary,
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M5" cross CrossVersion.full),
    javacOptions ++= Seq("-target", "1.6", "-source", "1.6"),
    scalacOptions in Compile <++= (Keys.`package` in (myma, Compile), resourceDirectory in (myma, Compile)) map { (jar: File, resources: File) =>
      System.setProperty("myma.whitelist.conf", resources.getAbsolutePath + "/whitelist.conf")
      System.setProperty("myma.entirelist.conf", resources.getAbsolutePath + "/entirelist.conf")
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    }
  )

  lazy val plugin = Project(
    id   = "paradise",
    base = file("plugin")
  ) settings (
    publishableSettings ++ assemblySettings : _*
  ) settings (
    resourceDirectory in Compile <<= baseDirectory(_ / "src" / "main" / "scala" / "org" / "scalamacros" / "paradise" / "embedded"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "provided"),
    test in assembly := {},
    jarName in assembly := name.value + "_" + scalaVersion.value + "-" + version.value + "-assembly.jar",
    assemblyOption in assembly ~= { _.copy(includeScala = false) },
    Keys.`package` in Compile := {
      val slimJar = (Keys.`package` in Compile).value
      val fatJar = new File(crossTarget.value + "/" + (jarName in assembly).value)
      val _ = assembly.value
      IO.copy(List(fatJar -> slimJar), overwrite = true)
      println("package: merged paradise and quasiquotes and produced a fat JAR")
      slimJar
    },
    packagedArtifact in Compile in packageBin := {
      val temp = (packagedArtifact in Compile in packageBin).value
      val (art, slimJar) = temp
      val fatJar = new File(crossTarget.value + "/" + (jarName in assembly).value)
      val _ = assembly.value
      IO.copy(List(fatJar -> slimJar), overwrite = true)
      println("packagedArtifact: merged paradise and quasiquotes and produced a fat JAR")
      (art, slimJar)
    }
  ) dependsOn (quasiquotes)

  lazy val usePluginSettings = Seq(
    scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      System.setProperty("macroparadise.plugin.jar", jar.getAbsolutePath)
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      // Thanks Jason for this cool idea (taken from https://github.com/retronym/boxer)
      // add plugin timestamp to compiler options to trigger recompile of
      // main after editing the plugin. (Otherwise a 'clean' is needed.)
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    }
  )

  lazy val sandbox = Project(
    id   = "sandbox",
    base = file("sandbox")
  ) settings (
    sharedSettings ++ usePluginSettings: _*
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
  ) dependsOn (quasiquotes)

  lazy val tests = Project(
    id   = "tests",
    base = file("tests")
  ) settings (
    sharedSettings ++ usePluginSettings: _*
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
    publishArtifact in Compile := false,
    unmanagedSourceDirectories in Test <<= (scalaSource in Test) { (root: File) =>
      // TODO: I haven't yet ported negative tests to SBT, so for now I'm excluding them
      val (anns :: Nil, others) = root.listFiles.toList.partition(_.getName == "annotations")
      val (negAnns, otherAnns) = anns.listFiles.toList.partition(_.getName == "neg")
      otherAnns ++ others
    },
    fullClasspath in Test := {
      val testcp = (fullClasspath in Test).value.files.map(_.getAbsolutePath).mkString(java.io.File.pathSeparatorChar.toString)
      sys.props("classpath.for.repl.tests") = testcp
      (fullClasspath in Test).value
    },
    scalacOptions ++= Seq()
    // scalacOptions ++= Seq("-Xprint:typer")
    // scalacOptions ++= Seq("-Xlog-implicits")
  ) dependsOn (quasiquotes)
}
