import sbt._
import Keys._

object Settings {
  lazy val java_home: File = if ("FreeBSD" == sys.env.getOrElse("OSTYPE", "")) file("/usr/local/openjdk11") else file("/usr/lib/jvm/default")
  lazy val java_lib: File = if ("FreeBSD" == sys.env.getOrElse("OSTYPE", "")) file("/usr/local/share/java/classes") else file("/usr/share/java/lib") 
  lazy val scala_home: File = if ("FreeBSD" == sys.env.getOrElse("OSTYPE", "")) file("/usr/local/share/scala") else file("/usr/share/scala")
  lazy val scala_lib: File = scala_home / "lib"
    
  lazy val depnMap = Map(
    "scalaLib" -> "org.scala-lang" % "scala-library" % "2.13.2"
    , "scalaTest" -> "org.scalatest" %% "scalatest" % "3.2.10"
    , "scalaCheck" -> "org.scalacheck" %% "scalacheck" % "1.15.4"
    , "scalaStyle" -> "com.beautiful-scala" %% "scalastyle" % "[1.4.0,)"
      
    , "junitConsole" -> "org.junit.platform" % "junit-platform-console" % "1.8.0"
    , "junitSuite" -> "org.junit.platform" % "junit-platform-suite" % "1.8.0"
    , "junitJupiter" -> "org.junit.jupiter" % "junit-jupiter" % "5.8.0"
    , "junitRunner" -> "org.junit.platform" % "junit-platform-runner" % "1.8.0"
    , "junitVintage" -> "org.junit.vintage" % "junit-vintage-engine" % "5.8.0"
    , "testng" -> "org.testng" % "testng" % "7.5"
    , "jacocoAnt" -> "org.jacoco" % "org.jacoco.ant" % "[0.8.5,)"
    , "checkStyle" -> "com.puppycrawl.tools" % "checkstyle" % "[8.33,)"
    
    , "slf4j" -> "org.slf4j" % "slf4j-api" % "1.7.33"
    , "log4jOverSlf4j" -> "org.slf4j" % "log4j-over-slf4j" % "1.7.33"
    , "logbackClassic" -> "ch.qos.logback" % "logback-classic" % "1.2.10"
    
    , "ini4j" -> "org.ini4j" % "ini4j" % "0.5.4"
    , "jna" -> "net.java.dev.jna" % "jna" % "5.10.0"
    
    , "jsonp" -> "org.glassfish" % "javax.json" % "1.1.4"
    , "jsonpApi" -> "javax.json" % "javax.json-api" % "1.1.4"
    , "snakeYaml" -> "org.yaml" % "snakeyaml" % "1.30"
    , "toml4j" -> "com.moandjiezana.toml" % "toml4j" % "0.7.2"
  )
  
  lazy val defaultSettings = Seq(
    autoScalaLibrary := false
    //, scalaModuleInfo ~= (_ map (_ withOverrideScalaVersion true))
    //, Global / offline := true
    //, update / skip := true
    //, retrieveManaged := false
    //, javaHome := Some(java_home)
    //, scalaHome := Some(scala_home)
    , crossPaths := false
    
    /*, resolvers += Seq(
      Resolver.url("Local Ivy Repository",
        "file://" + Path.userHome.absolutePath + "/.ivy2/local"
                )(Patterns("[organization]/[module]/[revision]/[artifact]-[revision](-[classifier]).[ext]"))
        //, Resolver.mavenLocal
        //, "Local Maven Repository" at
        //  "file://" + Path.userHome.absolutePath + "/.m2/repository"
    )*/
    , resolvers += "Local Ivy Repository" at "file:///%s/.ivy2/local".format(Path.userHome.absolutePath)
    //, resolvers += "Local Maven Repository" at "file:///%s/.m2/repository".format(Path.userHome.absolutePath)
    , resolvers += Resolver.mavenLocal
    //, resolvers += Resolver.jcenterRepo
    
    , envVars := Map("LD_LIBRARY_PATH" -> "%s:%s".
      format(sys.env.getOrElse("LD_LIBRARY_PATH", ".:/usr/local/lib"),
        sys.props.getOrElse("java.library.path", "")))
    , Test / run / fork := true
    , cleanFiles ++= { baseDirectory (_ * "*.log" get) value }
    
    , javaOptions ++= Seq("-esa", "-ea","-Xmx1024m", "-Xms16m", "-Xss16m", 
      "-Djava.library.path=%s".format(
        sys.props.getOrElse("java.library.path", "")))
    , javacOptions ++= Seq("-Xlint:all", "-deprecation") ++ (if ("debug" == sys.env.getOrElse("CONFIG", "")) Seq("-g") else Seq())
    , Compile / doc / javacOptions ++= Seq("-use", "-private", "-version",
      "-author")
      
    , scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xlint", 
      "-feature") ++ (if ("debug" == sys.env.getOrElse("CONFIG", "")) Seq("-g:vars") else Seq("-opt:l:method", "-opt:l:inline"))
    , Compile / doc / scalacOptions ++= Seq("-author")
    
    , Test/ packageBin / packageOptions += {
      val classpath: Classpath = (Test / dependencyClasspath) value;
      val filePaths = classpath map { attrFile: Attributed[File] => 
        //attrFile.data.toPath().toString() // absolute paths
        //target.toPath().relativize(attrFile.data.toPath()).
        //  toString()                      // relative paths
        "lib/" + attrFile.data.toPath().getFileName().toString()
        };
      Package.ManifestAttributes(java.util.jar.Attributes.Name.CLASS_PATH -> 
        filePaths.reduceOption(_ + " " + _).getOrElse(""))
      }
    , Compile / packageBin / packageOptions += {
      val classpath: Classpath = (Runtime / externalDependencyClasspath) value;
      val filePaths = classpath map { attrFile: Attributed[File] => 
        "lib/" + attrFile.data.toPath().getFileName().toString() }; 
      Package.ManifestAttributes(java.util.jar.Attributes.Name.CLASS_PATH -> 
        filePaths.reduceOption(_ + " " + _).getOrElse(""))
      }
    , exportJars := true
  )
}
