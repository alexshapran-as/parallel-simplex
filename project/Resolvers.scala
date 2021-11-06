import sbt.{Resolver, _}

object Resolvers {
    val typeSafeRepo = Classpaths.typesafeReleases
    val mavenRepo = Resolver.mavenLocal

    def commonResolvers = Seq(typeSafeRepo, mavenRepo)
}
