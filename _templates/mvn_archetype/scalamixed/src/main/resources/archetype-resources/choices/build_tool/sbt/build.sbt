#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
import Settings._
import BuildTasks._

ThisBuild / scalaVersion := "2.13.2"
ThisBuild / organization := "${groupId}"
ThisBuild / organizationName := "Coding Sandbox"

// ? need old functionality (useCoursier := false) f/ external ivy.xml / pom
ThisBuild / useCoursier := true

lazy val ${name} = project.in(file("."))
  .settings(defaultSettings:_*)
  .settings(otherTasks:_*)
  .settings(name := "${parent}-${name}"
    , version := "${version}"

    // ?? deprecated since sbt 1.5
    //, externalIvySettingsURL(url("file://" +
    //  sys.env.getOrElse("HOME", ".") + "/.ivy2/ivysettings.xml"))
    //, externalIvySettings(Def.setting(file(sys.env.getOrElse("HOME",
    //  ".") + "/.ivy2/ivysettings.xml")))
    //, externalIvyFile()

    //, libraryDependencies += "${groupId}" % "${parent}-bom" % "0" pomOnly()
    //, libraryDependencies += ("org.scalatest" %% "scalatest" % "3.2.10" % Test)
    //  .exclude("org.scala-lang.modules", "scala-xml_2.12")
    //  .exclude("org.scala-lang.modules", "scala-parser-combinators_2.12")
    , libraryDependencies ++= Seq(
      //"org.scala-lang" % "scala-library" % "2.13.2"
      //, "org.scalatest" %% "scalatest" % "3.2.10" % Test
      depnMap("scalaLib"), depnMap("scalaTest") % Test
      , depnMap("scalaCheck") % Test

      //, depnMap("junitConsole") % Test
      //, depnMap("junitSuite") % Test, depnMap("junitJupiter") % Test
      //, depnMap("junitRunner") % Test, depnMap("junitVintage") % Test
      //, depnMap("testng") % Test

      , depnMap("slf4j")//, depnMap("log4jOverSlf4j") % Runtime
      , depnMap("logbackClassic") % Runtime

      , depnMap("ini4j")
      //, depnMap("jna")
#{if}("yes" == ${executable})
      , depnMap("jsonp"), depnMap("jsonpApi")
      , depnMap("snakeYaml"), depnMap("toml4j")
#{end}
      )
    , Compile / classpathConfiguration := Compile
    , Runtime / classpathConfiguration := Runtime
    , Test / classpathConfiguration := Test
    //, publishArtifact := false
    //, unmanagedBase := baseDirectory.value / "lib"
    //, Test / unmanagedJars ++= (java_lib ** ("hamcrest-core.jar" ||
    //  "junit.jar")).classpath
    //, Compile / unmanagedJars ++= {
    //  val base = baseDirectory.value
    //  val baseDirectories = (base / "lib") +++ (base / "../lib")
    //  val customJars = (baseDirectories ** "*.jar") +++ (java_lib **
    //    ("slf4j-api.jar" || "jna.jar"))
    //  customJars.classpath }
    )
  .settings(Test / unmanagedSources / excludeFilter := "ClassicTest.*" || "ClassicProp.*")

//lazy val scalaCompat = sys.props.getOrElse("scala.compat", "2.13")

//addCommandAlias("install", "publishLocal")

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
