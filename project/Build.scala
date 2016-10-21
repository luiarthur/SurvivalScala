import sbt._

object MyBuild extends Build {
  lazy val root = Project("root", file(".")).dependsOn(mcmc_scala)
  lazy val mcmc_scala = RootProject(uri("git://github.com/luiarthur/mcmc_scala.git"))
}
