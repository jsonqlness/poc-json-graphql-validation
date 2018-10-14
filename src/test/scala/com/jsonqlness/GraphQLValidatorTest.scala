package com.jsonqlness

import com.jsonqlness.GraphQLValidator.{MissingFields, MissingValueTypeInSchema}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.TryValues._

class GraphQLValidatorTest extends FlatSpec {
  val basicSchema =
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
    """

  val jsonToGraphQLMapper = new JsonToGraphQLMapper()

  "GraphQLValidator" should "fail because of missing fields" in {
    val schema = (basicSchema +
      """
        |type Character {
        |    id: ID!
        |    name: String!
        |    appearsIn: [Episode]!
        |    isLegacy: Boolean!
        |}
      """).stripMargin

    val character = """{"name": "Stefan"}""".stripMargin

    new GraphQLValidator(jsonToGraphQLMapper, schema).apply(character, "Character").failure.exception shouldBe MissingFields(Seq("id", "appearsIn", "isLegacy"))
  }

  it should "fail only with nonNull fields" in {
    val schema = (basicSchema +
      """
        |type Character {
        |    id: ID!
        |    name: String!
        |    appearsIn: [Episode]
        |    isLegacy: Boolean!
        |}
      """).stripMargin

    val character = """{"name": "Stefan"}""".stripMargin

    new GraphQLValidator(jsonToGraphQLMapper, schema).apply(character, "Character").failure.exception shouldBe MissingFields(Seq("id", "isLegacy"))
  }

  it should "pass if all the fields are in JSON object" in {
    val schema = (basicSchema +
      """
        |type Character {
        |    id: ID!
        |    name: String!
        |    appearsIn: [Episode]!
        |    isLegacy: Boolean!
        |}
      """).stripMargin

    val character =
      """{
          |"id": 123,
          |"name": "Stefan",
          |"appearsIn": "JEDI",
          |"isLegacy": true
          |}""".stripMargin

    new GraphQLValidator(jsonToGraphQLMapper, schema).apply(character, "Character").success.value shouldBe ()
  }

  it should "pass if all nonNull fields are in JSON object" in {
    val schema = (basicSchema +
      """
        |type Character {
        |    id: ID!
        |    name: String!
        |    appearsIn: [Episode]
        |    isLegacy: Boolean!
        |}
      """).stripMargin

    val character =
      """{
        |"id": 123,
        |"name": "Stefan",
        |"isLegacy": true
        |}""".stripMargin

    new GraphQLValidator(jsonToGraphQLMapper, schema).apply(character, "Character").success.value shouldBe ()
  }

  it should "fail with MissingValueTypeInSchema error when there is missing type in a schema" in {
    val schema = (basicSchema +
      """
        |type Character {
        |    id: ID!
        |}
      """).stripMargin

    new GraphQLValidator(jsonToGraphQLMapper, schema).apply("{}", "Character2").failure.exception shouldBe a [MissingValueTypeInSchema]
  }
}
