name := "paper-computer"

version := "0.7"

// https://www.scala-lang.org/download/all.html
scalaVersion := "2.12.9"

// https://typelevel.org/cats/
libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"

// http://www.scalatest.org/install
// not used yet libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

// https://www.scalacheck.org/
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % Test

// not needed as I do functional programming and property based testing
// scalamock siehe http://scalamock.org/quick-start/
// libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % Test

// https://github.com/fthomas/refined
libraryDependencies ++= Seq(
  "eu.timepit" %% "refined" % "0.9.19",
  "eu.timepit" %% "refined-cats" % "0.9.19", // optional
  "eu.timepit" %% "refined-eval" % "0.9.19" // optional, JVM-only
)

// https://fs2.io/
libraryDependencies += "co.fs2" %% "fs2-core" % "2.4.6"

scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8", // Specify character encoding used by source files.
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-language:higherKinds",
  "-Ypartial-unification",
  "-Xfuture", // Turn on future language features.
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused",
  "-Xfatal-warnings", // Fail the compilation if there are any warnings.
  "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
  "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  "-Xlint:option-implicit", // Option.apply used implicit view.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match", // Pattern match may not be typesafe.
  //"-Yrecursion:123" // TODO maybe I need type recursion is needed in the future?
)
