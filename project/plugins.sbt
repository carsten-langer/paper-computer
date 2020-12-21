// not used yet
// http://www.scalatest.org/install
// http://repo.artima.com/releases/com/artima/supersafe/
//resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
//addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.8")

// https://scalacenter.github.io/scalafix/docs/users/installation.html
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.24")

// https://github.com/fiadliel/sbt-partial-unification
addSbtPlugin("org.lyranthe.sbt" % "partial-unification" % "1.1.2")

// https://github.com/typelevel/kind-projector
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.2" cross CrossVersion.full)

// https://stryker-mutator.io/docs/stryker4s/getting-started/
addSbtPlugin("io.stryker-mutator" % "sbt-stryker4s" % "0.10.0")
