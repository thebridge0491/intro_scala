#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
import Settings._
import BuildTasks._

ThisBuild / scalaVersion := "2.13.2"
ThisBuild / organization := "${groupId}"
ThisBuild / organizationName := "Coding Sandbox"
ThisBuild / version := "${version}"

lazy val root = project.in(file("."))
  .settings(defaultSettings:_*)
  .settings(name := "${parent}"
    , Test / run := { (Test / run / util).evaluated }
    , Compile / run := { () }
    ).aggregate(parent, util, intro)

lazy val parent = project.in(file("parent"))

lazy val util = project.in(file("common"))

lazy val intro = project.in(file("app"))
  //.dependsOn(util)
