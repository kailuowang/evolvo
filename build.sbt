import com.typesafe.sbt.SbtGit.git

addCommandAlias(
  "gitSnapshots",
  ";set version in ThisBuild := git.gitDescribedVersion.value.get + \"-SNAPSHOT\""
)

val apache2 = "Apache-2.0" -> url(
  "https://www.apache.org/licenses/LICENSE-2.0.html"
)
val gh = GitHubSettings(
  org = "kailuowang",
  proj = "evolvo",
  publishOrg = "com.kailuowang",
  license = apache2
)

val mainDev =
  Developer(
    "Kailuo Wang",
    "@kailuowang",
    "kailuo.wang@gmail.com",
    new java.net.URL("http://github.com/kailuowang")
  )

val devs = List(Developer)

lazy val libs =
  org.typelevel.libraries
    .add("munit", version = "0.7.7", org = "org.scalameta", "munit-scalacheck")

val root = project
  .in(file("."))
  .dependsOn(core)
  .aggregate(core)
  .settings(commonSettings)

lazy val core = project
  .settings(
    commonSettings,
    libs.dependencies("cats-core"),
    libs.testDependencies("munit-scalacheck")
  )

lazy val buildSettings = sharedBuildSettings(gh, libs)
lazy val publishSettings = sharedPublishSettings(gh) ++ credentialSettings ++ sharedReleaseProcess

lazy val commonSettings = buildSettings ++
  addCompilerPlugins(libs, "kind-projector") ++ sharedCommonSettings ++ scalacAllSettings ++ Seq(
  organization := "com.kailuowang",
  parallelExecution in Test := false,
  scalaVersion := libs.vers("scalac_2.13"),
  testFrameworks += new TestFramework("munit.Framework")
) ++ publishSettings
