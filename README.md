## Macro Paradise Plugin

This plugin is no longer actively developed, but we do continue to release a new version whenever a new Scala 2.12.x version comes out.

In Scala 2.13, the plugin's functionality has been included in the compiler directly under the `-Ymacro-annotations` flag. (Regardless, its status remains the same: experimental, API subject to change.)

[http://docs.scala-lang.org/overviews/macros/paradise.html](http://docs.scala-lang.org/overviews/macros/paradise.html)

[![Join the chat at https://gitter.im/scalamacros/paradise](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scalamacros/paradise?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

### Cutting a release

Do it on your own machine. The GPG key is used for scala modules, talk to [@lrytz](https://github.com/lrytz/) if you need the passphrase.

Update the `scalaVersion` in the build, then

```
$> java -version
openjdk version "1.8.0_222"
OpenJDK Runtime Environment (AdoptOpenJDK)(build 1.8.0_222-b10)
OpenJDK 64-Bit Server VM (AdoptOpenJDK)(build 25.222-b10, mixed mode)

$> export SONA_USER=⛄️ SONA_PASS=🌨

$> sbt \
  'set version := "2.1.1"' \
  'set pgpSigningKey := Some(new java.math.BigInteger("C478A820AD150412FF2860C563426A08B91ED6B0", 16).longValue)' \
  'set pgpPassphrase := Some(Array.empty)' \
  'set useGpg := true' \
  clean \
  publishSigned

$> git tag -s -m "2.1.1 for Scala 2.12.XY" v2.1.1_2.12.XY HEAD

$> git push upstream v2.1.1_2.12.XY
```

Then you'll need to close and release the staging repo, via https://oss.sonatype.org/#stagingRepositories, or with a command-line tool if you prefer.
