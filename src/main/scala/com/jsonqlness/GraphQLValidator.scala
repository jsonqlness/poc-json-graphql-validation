package com.jsonqlness

import com.jsonqlness.GraphQLValidator.MissingFields
import graphql.schema.idl.{RuntimeWiring, SchemaGenerator, SchemaParser}

import scala.util.Try
import scala.collection.JavaConverters._
import graphql.schema.GraphQLTypeUtil.isNonNull


class GraphQLValidator(jsonToGraphQLMapper: JsonToGraphQLMapper, schemaStringify: String) {
  private val typesRegistry = new SchemaParser().parse(schemaStringify)

  private val schema = new SchemaGenerator().makeExecutableSchema(typesRegistry, RuntimeWiring.newRuntimeWiring().build())

  def apply(jsonString: String, validationType: String): Try[Unit] = Try {

    val objectValue = jsonToGraphQLMapper.map(jsonString)

    val fields = schema
      .getObjectType(validationType)
      .getFieldDefinitions
      .asScala

    val missingFields = fields
      .filter(f => isNonNull(f.getType))
      .filterNot(q => objectValue.getObjectFields.asScala.exists(p => p.getName == q.getName))

    if (missingFields.nonEmpty)
      throw MissingFields(missingFields.map(_.getName))
  }
}

object GraphQLValidator {
  sealed trait ValidationError extends IllegalStateException
  case class MissingFields(fields: Seq[String]) extends ValidationError {
    override def toString: String = fields.toString
  }
}