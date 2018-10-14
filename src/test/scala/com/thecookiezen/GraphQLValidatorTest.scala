package com.thecookiezen

import com.thecookiezen.GraphQLValidator.MissingFields
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.TryValues._

class GraphQLValidatorTest extends FlatSpec {
  val schema =
    """
      |schema {
      |    query: QueryType
      |}
      |
      |type QueryType {
      |    hero(episode: Episode): Character
      |}
      |
      |
      |enum Episode {
      |    NEWHOPE
      |    EMPIRE
      |    JEDI
      |}
      |
      |type Character {
      |    id: ID!
      |    name: String!
      |    appearsIn: [Episode]!
      |    isLegacy: Boolean!
      |}
    """.stripMargin

  val jsonToGraphQLMapper = new JsonToGraphQLMapper()
  val graphQLValidator = new GraphQLValidator(jsonToGraphQLMapper, schema)

  "GraphQLValidator" should "fail because of missing fields" in {
    val character = """{"name": "Stefan"}""".stripMargin

    graphQLValidator.apply(character, "Character").failure.exception shouldBe MissingFields(Seq("id", "appearsIn", "isLegacy"))
  }
}