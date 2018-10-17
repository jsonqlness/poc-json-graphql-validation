package com.jsonqlness

import com.jsonqlness.GraphQLValidator.{MissingFields, MissingValueTypeInSchema, NonNullValueFailure}
import graphql.language.{NullValue, ObjectValue, Value}
import graphql.schema.GraphQLTypeUtil.isNonNull
import graphql.schema.idl.{RuntimeWiring, SchemaGenerator, SchemaParser}
import graphql.schema.{GraphQLObjectType, GraphQLTypeUtil}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

class GraphQLValidator(jsonToGraphQLMapper: JsonToGraphQLMapper, schemaStringify: String) {

  private val typesRegistry = new SchemaParser().parse(schemaStringify)

  private val schema =
    new SchemaGenerator().makeExecutableSchema(typesRegistry, RuntimeWiring.newRuntimeWiring().build())

  def apply(jsonString: String, validationType: String): Try[Unit] = {
    val objectValue = jsonToGraphQLMapper.map(jsonString)

    Option(schema.getObjectType(validationType))
      .map(Success(_))
      .getOrElse(Failure(MissingValueTypeInSchema(validationType, schema.getAllTypesAsList.asScala.map(_.getName))))
      .map(checkNonNull(objectValue, _))
      .map(checkMissingFields(objectValue, _))
      .map(GraphQLTypeUtil.unwrapOne)
      .map(_ => ())
  }

  private def checkNonNull(value: Value[_], objectType: GraphQLObjectType) = {
    if (value == null || value.isInstanceOf[NullValue]) {
      if (isNonNull(objectType))
        throw NonNullValueFailure(objectType.toString)
    }
    objectType
  }

  private def checkMissingFields(value: ObjectValue, objectType: GraphQLObjectType) = {
    val missingFields = objectType.getFieldDefinitions.asScala
      .filter(f => isNonNull(f.getType))
      .filterNot(q => value.getObjectFields.asScala.exists(p => p.getName == q.getName))

    if (missingFields.nonEmpty)
      throw MissingFields(missingFields.map(_.getName))

    objectType
  }
}

object GraphQLValidator {

  sealed trait ValidationError extends IllegalStateException

  case class MissingFields(fields: Seq[String]) extends ValidationError {
    override def toString: String = fields.toString
  }

  case class MissingValueTypeInSchema(missingType: String, allSchemaTypes: Seq[String]) extends ValidationError {
    override def toString: String =
      s"There is an missing type $missingType in a schema: $allSchemaTypes"
  }

  case class NonNullValueFailure(nonNullType: String) extends ValidationError {
    override def toString: String =
      s"Value of type [$nonNullType] should not be null or instance of NullValue"
  }

}
