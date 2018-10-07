package com.thecookiezen

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node._
import graphql.language._

import scala.collection.JavaConverters._

class JsonToGraphQLMapper(mapper: ObjectMapper = new ObjectMapper()) {

  def map(jsonInput: String): ObjectValue = mapJsonToObjectValue(mapper.readTree(jsonInput))

  private def mapJsonToObjectValue(objectNode: JsonNode): ObjectValue = {
    val objectFields: Iterator[ObjectField] = objectNode.fields().asScala.map(entry => {
      val o = ObjectField
        .newObjectField()
        .name(entry.getKey)

      o.value(mapPrimitiveJsonNodeToValue(entry.getValue)).build()
    })

    ObjectValue.newObjectValue().objectFields(objectFields.toList.asJava).build()
  }

  private def mapPrimitiveJsonNodeToValue(jsonNode: JsonNode): Value[_ <: Value[_ <: AnyRef]] = jsonNode match {
    case objectNode: ObjectNode => mapJsonToObjectValue(objectNode)
    case node: ArrayNode => ArrayValue
      .newArrayValue()
      .values(node.asScala.map(n => mapPrimitiveJsonNodeToValue(n)).toList.asJava)
      .build()
    case node: BooleanNode => new BooleanValue(node.booleanValue())
    case node: IntNode => new IntValue(node.bigIntegerValue())
    case node: DoubleNode => new FloatValue(node.decimalValue())
    case node: LongNode => new IntValue(node.bigIntegerValue())
    case node: FloatNode => new FloatValue(node.decimalValue())
    case node: TextNode => new StringValue(node.textValue())
    case _ => NullValue.Null
  }
}
