package com.jsonqlness

import graphql.language._
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.collection.JavaConverters._

class JsonToGraphQLMappingTest extends FlatSpec {

  private val mapper = new JsonToGraphQLMapper()

  "Json2GrapQLMapper" should "map empty json to empty ObjectValue" in {
    mapper.map("{}") shouldBe a [ObjectValue]
  }

  it should "parse simple json object with string field" in {
    mapper.map("""{"name": "John"}""") should hasAllFields(Seq(
      ObjectField.newObjectField().name("name").value(new StringValue("John")).build()
    ))
  }

  it should "parse simple json object with integer field" in {
    mapper.map("""{"name": 123}""") should hasAllFields(Seq(
      ObjectField.newObjectField().name("name").value(new IntValue(BigInt(123).bigInteger)).build()
    ))
  }

  it should "parse simple json object with boolean field" in {
    mapper.map("""{"isTrue": false}""") should hasAllFields(Seq(
      ObjectField.newObjectField().name("isTrue").value(new BooleanValue(false)).build()
    ))
  }

  it should "parse simple json object with null field" in {
    mapper.map("""{"nullable": null}""") should hasAllFields(Seq(
      ObjectField.newObjectField().name("nullable").value(NullValue.Null).build()
    ))
  }

  it should "parse simple json object with float field" in {
    mapper.map("""{"price": 1.34}""") should hasAllFields(Seq(
      ObjectField.newObjectField().name("price").value(new FloatValue(BigDecimal(1.34).bigDecimal)).build()
    ))
  }

  it should "parse simple json object with array of strings field" in {
    mapper.map("""{"stringList": ["a","b","c"]}""") should hasAllFields(Seq(
      ObjectField.newObjectField().name("stringList").value(
        ArrayValue.newArrayValue().values(java.util.List.of(new StringValue("a"), new StringValue("b"), new StringValue("c"))).build()
      ).build()
    ))
  }

  it should "parse simple json object with array of booleans field" in {
    mapper.map("""{"booleanList": [false, false, true]}""") should hasAllFields(Seq(
      ObjectField.newObjectField().name("booleanList").value(
        ArrayValue.newArrayValue().values(java.util.List.of(new BooleanValue(false), new BooleanValue(false), new BooleanValue(true))).build()
      ).build()
    ))
  }

  it should "parse complex json object with array of booleans field, one string field and one float field" in {
    mapper.map(
      """{
        | "price": 1.34,
        | "title": "New Hope",
        | "booleanList": [false, false, true]
        |}""".stripMargin) should hasAllFields(Seq(
      ObjectField.newObjectField().name("price").value(new FloatValue(BigDecimal(1.34).bigDecimal)).build(),
      ObjectField.newObjectField().name("title").value(new StringValue("New Hope")).build(),
      ObjectField.newObjectField().name("booleanList").value(
        ArrayValue.newArrayValue().values(java.util.List.of(new BooleanValue(false), new BooleanValue(false), new BooleanValue(true))).build()
      ).build()
    ))
  }

  it should "parse nested object" in {
    mapper.map(
      """{
        | "price": 1.34,
        | "title": "New Hope",
        | "cast": {
        |   "firstPlan" : "Luke",
        |   "secondPlan": "Vader"
        | }
        |}""".stripMargin) should hasAllFields(Seq(
      ObjectField.newObjectField().name("price").value(new FloatValue(BigDecimal(1.34).bigDecimal)).build(),
      ObjectField.newObjectField().name("title").value(new StringValue("New Hope")).build(),
      ObjectField.newObjectField().name("cast").value(
        ObjectValue.newObjectValue().objectFields(Seq(
          ObjectField.newObjectField().name("firstPlan").value(new StringValue("Luke")).build(),
          ObjectField.newObjectField().name("secondPlan").value(new StringValue("Vader")).build()
        ).asJava).build()
      ).build()
    ))
  }

  it should "parse more complex nested object" in {
    mapper.map(
      """{
        | "price": 1.34,
        | "title": "New Hope",
        | "cast": {
        |   "firstPlan" : {
        |     "hero": "Luke Skywalker",
        |     "actorName": "Mark Hamil"
        |   },
        |   "secondPlan": {
        |     "hero": "Han Solo",
        |     "actorName": "Harrison Ford"
        |   },
        |   "other": ["Chewie","Leia","Vader"]
        | }
        |}""".stripMargin) should hasAllFields(Seq(
      ObjectField.newObjectField().name("price").value(new FloatValue(BigDecimal(1.34).bigDecimal)).build(),
      ObjectField.newObjectField().name("title").value(new StringValue("New Hope")).build(),
      ObjectField.newObjectField().name("cast").value(
        ObjectValue.newObjectValue().objectFields(Seq(
          ObjectField.newObjectField().name("firstPlan").value(ObjectValue.newObjectValue().objectFields(Seq(
            ObjectField.newObjectField().name("hero").value(new StringValue("Luke Skywalker")).build(),
            ObjectField.newObjectField().name("actorName").value(new StringValue("Mark Hamil")).build()
          ).asJava).build()).build(),
          ObjectField.newObjectField().name("secondPlan").value(ObjectValue.newObjectValue().objectFields(Seq(
            ObjectField.newObjectField().name("hero").value(new StringValue("Han Solo")).build(),
            ObjectField.newObjectField().name("actorName").value(new StringValue("Harrison Ford")).build()
          ).asJava).build()).build(),
          ObjectField.newObjectField().name("other").value(
            ArrayValue.newArrayValue().values(java.util.List.of(new StringValue("Chewie"), new StringValue("Leia"), new StringValue("Vader"))).build()
          ).build()
        ).asJava).build()
      ).build()
    ))
  }

  it should "parse array of objects" in {
    mapper.map(
      """{
        | "movies": [
        |   {
        |     "title": "New Hope",
        |     "year": 1987,
        |     "score": 9.8
        |   },
        |   {
        |     "title": "Empire Strikes Back",
        |     "year": 1992,
        |     "score": 9.5
        |   },
        |   {
        |     "title": "The Last Jedi",
        |     "year": 2017,
        |     "score": 8.1
        |   }
        | ]
        |}""".stripMargin) should hasAllFields(Seq(
      ObjectField.newObjectField().name("movies").value(
        ArrayValue.newArrayValue().values(java.util.List.of(
          ObjectValue.newObjectValue().objectFields(Seq(
            ObjectField.newObjectField().name("title").value(new StringValue("New Hope")).build(),
            ObjectField.newObjectField().name("year").value(new IntValue(BigInt(1987).bigInteger)).build(),
            ObjectField.newObjectField().name("score").value(new FloatValue(BigDecimal(9.8).bigDecimal)).build()
          ).asJava).build(),
          ObjectValue.newObjectValue().objectFields(Seq(
            ObjectField.newObjectField().name("title").value(new StringValue("Empire Strikes Back")).build(),
            ObjectField.newObjectField().name("year").value(new IntValue(BigInt(1992).bigInteger)).build(),
            ObjectField.newObjectField().name("score").value(new FloatValue(BigDecimal(9.5).bigDecimal)).build()
          ).asJava).build(),
          ObjectValue.newObjectValue().objectFields(Seq(
            ObjectField.newObjectField().name("title").value(new StringValue("The Last Jedi")).build(),
            ObjectField.newObjectField().name("year").value(new IntValue(BigInt(2017).bigInteger)).build(),
            ObjectField.newObjectField().name("score").value(new FloatValue(BigDecimal(8.1).bigDecimal)).build()
          ).asJava).build()
        )).build()
      ).build()
    ))
  }

  private def hasAllFields(expected: Seq[ObjectField]) = Matcher[ObjectValue] (actual => MatchResult(
    expected.forall { expectedField =>
      actual.getObjectFields.asScala.exists(actualField =>
        actualField.isEqualTo(expectedField) && compareValues(expectedField, actualField)
      )
    }, s"not contain $expected fields 1, $actual", s"not contain $expected fields 2"
  ))

  private def compareValues(expectedField: ObjectField, actualField: ObjectField): Boolean = {
    (expectedField.getValue, actualField.getValue) match {
      case (expected: ArrayValue, actual: ArrayValue) => expected.getValues.asScala.zip(actual.getValues.asScala)
        .forall(entries => entries._1.isEqualTo(entries._2))
      case _ => actualField.getValue.isEqualTo(expectedField.getValue)
    }
  }
}
