#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
import Settings._
import BuildTasks._

//scalaVersion     in ThisBuild := "2.9.2"
organization     in ThisBuild := "${groupId}"
organizationName in ThisBuild := "Coding Sandbox"

lazy val ${name} = (project in file("."))
	.settings(defaultSettings:_*)
	.settings(otherTasks:_*)
	.settings(name := "${parent}-${name}"
        , version := "${version}"
        /*, libraryDependencies ++= Seq(
            "org.scala-lang" % "scala-library" % "[2.9.2,)"
            , "junit" % "junit" % "[4.10,)" % "test"
			)*/
        //, externalIvySettingsURL(url("file://" + 
        //    sys.env.getOrElse("HOME", ".") + "/.ivy2/ivysettings.xml"))
        , externalIvySettings(Def.setting(file(sys.env.getOrElse("HOME", 
            ".") + "/.ivy2/ivysettings.xml")))
        , externalIvyFile()
        , classpathConfiguration in Compile := Compile
        , classpathConfiguration in Runtime := Runtime
        , classpathConfiguration in Test := Test
		//, publishArtifact := false
		//, unmanagedBase := baseDirectory.value / "lib"
		/*, unmanagedJars in Test ++= (java_lib ** ("hamcrest-core.jar" ||
            "junit.jar")).classpath
		, unmanagedJars in Compile ++= { 
            val base = baseDirectory.value
			val baseDirectories = (base / "lib") +++ (base / "../lib")
			val customJars = (baseDirectories ** "*.jar") +++ (java_lib ** 
				("slf4j-api.jar" || "jna.jar"))
			customJars.classpath }
        */)
	.settings(excludeFilter in Test in unmanagedSources := "ClassicTest.*" || "ClassicProp.*")

//lazy val scalaCompat = sys.props.getOrElse("scala.compat", "2.9")
