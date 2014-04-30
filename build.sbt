name := "Transphoner"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache
)

play.Project.playScalaSettings

lazy val transphonerWebApp = project.in(file("."))
    .aggregate(libtransphone)
    .dependsOn(libtransphone)
    
lazy val libtransphone = project