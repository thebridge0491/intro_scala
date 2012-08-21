#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
import Settings._

//scalaVersion     in ThisBuild := "2.9.2"
organization     in ThisBuild := "${groupId}"
organizationName in ThisBuild := "Coding Sandbox"

lazy val parent = (project in file("."))
	.settings(defaultSettings:_*)
	.settings(name := "${parent}-parent"
        , version := "0"
        , run in Test := { () }
        , run in Compile := { () }
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
		, publishArtifact := false
		)

//lazy val scalaCompat = sys.props.getOrElse("scala.compat", "2.9")
