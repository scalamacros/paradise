import sbt._
import Keys._

object build extends Build {
  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.12.0-M3",
    crossVersion := CrossVersion.full,
    version := "2.1.0-SNAPSHOT",
    organization := "org.scalamacros",
    description := "Empowers production Scala compiler with latest macro developments",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    scalacOptions ++= Seq("-deprecation", "-feature"),
    parallelExecution in Test := false, // hello, reflection sync!!
    logBuffered := false,
    scalaHome := {
      val scalaHome = System.getProperty("paradise.scala.home")
      if (scalaHome != null) {
        println(s"Going for custom scala home at $scalaHome")
        Some(file(scalaHome))
      } else None
    }
  )

  def loadCredentials(): List[Credentials] = {
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

  lazy val plugin = Project(
    id   = "paradise",
    base = file("plugin")
  ) settings (
    sharedSettings : _*
  ) settings (
    resourceDirectory in Compile <<= baseDirectory(_ / "src" / "main" / "scala" / "org" / "scalamacros" / "paradise" / "embedded"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-library" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    // TODO: how to I make this recursion work?
    // run <<= run in Compile in sandbox,
    // test <<= test in Test in tests
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
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
    ),
    credentials ++= loadCredentials()
  )

  lazy val usePluginSettings = Seq(
    scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      System.setProperty("sbt.paths.plugin.jar", jar.getAbsolutePath)
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
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    publishArtifact in Compile := false
  )

  lazy val tests = Project(
    id   = "tests",
    base = file("tests")
  ) settings (
    sharedSettings ++ usePluginSettings: _*
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies += "org.scalatest" % "scalatest_2.11.0-M3" % "1.9.1b" % "test",
    libraryDependencies += "org.scalacheck" % "scalacheck_2.11" % "1.10.2-SNAPSHOT" % "test",
    publishArtifact in Compile := false,
    unmanagedSourceDirectories in Test <<= (scalaSource in Test) { (root: File) =>
      // TODO: I haven't yet ported negative tests to SBT, so for now I'm excluding them
      val (anns :: Nil, others) = root.listFiles.toList.partition(_.getName == "annotations")
      val (negAnns, otherAnns) = anns.listFiles.toList.partition(_.getName == "neg")
      System.setProperty("sbt.paths.tests.scaladoc", anns.listFiles.toList.filter(_.getName == "scaladoc").head.getAbsolutePath)
      otherAnns ++ others
    },
    fullClasspath in Test := {
      val testcp = (fullClasspath in Test).value.files.map(_.getAbsolutePath).mkString(java.io.File.pathSeparatorChar.toString)
      sys.props("sbt.paths.tests.classpath") = testcp
      (fullClasspath in Test).value
    },
    scalacOptions ++= Seq()
    // scalacOptions ++= Seq("-Xprint:typer")
    // scalacOptions ++= Seq("-Xlog-implicits")
  )
}
