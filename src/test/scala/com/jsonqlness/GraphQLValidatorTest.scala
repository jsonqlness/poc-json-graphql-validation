package com.jsonqlness

import com.jsonqlness.GraphQLValidator.MissingFields
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

  it should "pass if all the fields are in JSON object" in {
    val character =
      """{
          |"id": 123,
          |"name": "Stefan",
          |"appearsIn": "JEDI",
          |"isLegacy": true
          |}""".stripMargin

    graphQLValidator.apply(character, "Character").success.value shouldBe ()
  }
}
