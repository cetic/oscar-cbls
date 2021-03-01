resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.10.1")//"0.6.8")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.1")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")