resolvers ++= Seq(Resolver.typesafeRepo("releases"),
  "Artima Maven Repository" at "https://repo.artima.com/releases"
)

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.7")
addSbtPlugin("com.codacy" % "sbt-codacy-coverage" % "2.3")
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")
addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.7")
addSbtPlugin("com.updateimpact" % "updateimpact-sbt-plugin" % "2.1.3")
