lazy val sharedSettings = Seq(
  scalaVersion  := "2.12.19",
  scalaHome     := Option(System.getProperty("paradise.scala.home")).map(file(_)),
  scalacOptions ++= Seq("-deprecation", "-feature"),

  version       := "2.1.1",
  crossVersion  := CrossVersion.full,
  organization  := "org.scalamacros",
  description   := "Empowers production Scala compiler with latest macro developments",

  resolvers     += Resolver.sonatypeRepo("snapshots"),
  resolvers     += Resolver.sonatypeRepo("releases"),
  resolvers     += "Sonatype staging" at "https://oss.sonatype.org/content/repositories/staging/",

  parallelExecution in Test := false, // hello, reflection sync!!
  logBuffered               := false,

  useGpg := true
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
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test",

  scalacOptions += "-Ywarn-unused-import",
  scalacOptions += "-Xfatal-warnings",

  publishArtifact in Compile := false,

  unmanagedSourceDirectories in Test := {
    // TODO: I haven't yet ported negative tests to SBT, so for now I'm excluding them
    val (anns :: Nil, others) = (scalaSource in Test).value.listFiles.toList.partition(_.getName == "annotations")
    val (negAnns, otherAnns) = anns.listFiles.toList.partition(_.getName == "neg")
    System.setProperty("sbt.paths.tests.scaladoc", anns.listFiles.toList.filter(_.getName == "scaladoc").head.getAbsolutePath)
    otherAnns ++ others
  },
  fullClasspath in Test := {
    val testcp = (fullClasspath in Test).value.files.map(_.getAbsolutePath).mkString(java.io.File.pathSeparatorChar.toString)
    sys.props("sbt.paths.tests.classpath") = testcp
    (fullClasspath in Test).value
  }
)
