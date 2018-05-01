lazy val sharedSettings = Seq(
  scalaVersion  := "2.13.0-pre-1c56f0a",
  scalaHome     := Option(System.getProperty("paradise.scala.home")).map(file(_)),
  scalacOptions ++= Seq("-deprecation", "-feature"),

  version       := "2.1.0-SNAPSHOT",
  crossVersion  := CrossVersion.full,
  organization  := "org.scalamacros",
  description   := "Empowers production Scala compiler with latest macro developments",

  resolvers     += Resolver.sonatypeRepo("snapshots"),
  resolvers     += Resolver.sonatypeRepo("releases"),
  resolvers     += "Sonatype staging" at "https://oss.sonatype.org/content/repositories/staging/",
  resolvers     += "scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/",

  parallelExecution in Test := false, // hello, reflection sync!!
  logBuffered               := false
)

def sonaCredentials: Option[Credentials] =
  for {
    sonaUser <- Option(System.getenv("SONA_USER"))
    sonaPass <- Option(System.getenv("SONA_PASS"))
   } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org",sonaUser, sonaPass)

lazy val plugin = Project(
  id   = "paradise",
  base = file("plugin")
) settings (
  sharedSettings : _*
) settings (
  resourceDirectory in Compile := baseDirectory.value / "src" / "main" / "scala" / "org" / "scalamacros" / "paradise" / "embedded",

  libraryDependencies += "org.scala-lang" % "scala-library"  % scalaVersion.value,
  libraryDependencies += "org.scala-lang" % "scala-reflect"  % scalaVersion.value,
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,

  publishMavenStyle := true,
  publishArtifact in Test := false,

  publishTo := Some {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT")) "snapshots" at nexus + "content/repositories/snapshots"
    else "releases" at nexus + "service/local/staging/deploy/maven2"
  },

  credentials ++= sonaCredentials.toSeq,
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

lazy val usePluginSettings = Seq(
  scalacOptions in Compile ++= {
    val jar = (Keys.`package` in (plugin, Compile)).value
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
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  publishArtifact in Compile := false
)

lazy val tests = Project(
  id   = "tests",
  base = file("tests")
) settings (
  sharedSettings ++ usePluginSettings: _*
) settings (
  libraryDependencies += "org.scala-lang" % "scala-reflect"  % scalaVersion.value,
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,

  libraryDependencies += "junit" % "junit" % "4.12" % "test",
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
  testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v"),


  scalacOptions += "-Ywarn-unused-import",
  scalacOptions += "-Xfatal-warnings",

  publishArtifact in Compile := false,

  fullClasspath in Test := {
    val testcp = (fullClasspath in Test).value.files.map(_.getAbsolutePath).mkString(java.io.File.pathSeparatorChar.toString)
    sys.props("sbt.paths.tests.classpath") = testcp
    (fullClasspath in Test).value
  }
)
