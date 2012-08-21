#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
import Settings._
import BuildTasks._

//scalaVersion     in ThisBuild := "2.9.2"
version          in ThisBuild := "${version}"
organization     in ThisBuild := "${groupId}"
organizationName in ThisBuild := "Coding Sandbox"

lazy val root = (project in file("."))
	.settings(defaultSettings:_*)
    .settings(name := "${parent}"
        , run in Test := { (run in util in Test).evaluated }
        , run in Compile := { () }
        )
	.aggregate(parent, util, intro)

lazy val parent = (project in file("parent"))

lazy val util = (project in file("common"))

lazy val intro = (project in file("app"))
	//.dependsOn(util)
