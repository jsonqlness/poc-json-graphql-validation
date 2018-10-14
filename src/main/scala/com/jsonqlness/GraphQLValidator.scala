package com.jsonqlness

import com.jsonqlness.GraphQLValidator.{MissingFields, MissingValueTypeInSchema}
import graphql.language.ObjectValue
import graphql.schema.GraphQLFieldDefinition
import graphql.schema.idl.{RuntimeWiring, SchemaGenerator, SchemaParser}

import scala.util.{Failure, Success, Try}
import scala.collection.JavaConverters._
import graphql.schema.GraphQLTypeUtil.isNonNull

import scala.collection.mutable
import scala.util.control.NonFatal


class GraphQLValidator(jsonToGraphQLMapper: JsonToGraphQLMapper, schemaStringify: String) {
  private val typesRegistry = new SchemaParser().parse(schemaStringify)

  private val schema = new SchemaGenerator().makeExecutableSchema(typesRegistry, RuntimeWiring.newRuntimeWiring().build())

  def apply(jsonString: String, validationType: String): Try[Unit] = Try {

    val objectValue = jsonToGraphQLMapper.map(jsonString)

    getTypeFields(validationType).map(findMissingFields(_, objectValue)) match {
      case Success(missingFields) => if (missingFields.nonEmpty)
        throw MissingFields(missingFields.map(_.getName))
      case Failure(exception) => throw exception
    }
  }

  private def findMissingFields(fields: Seq[GraphQLFieldDefinition], value: ObjectValue) = {
    fields
      .filter(f => isNonNull(f.getType))
      .filterNot(q => value.getObjectFields.asScala.exists(p => p.getName == q.getName))
  }

  private def getTypeFields(validationType: String) = {
    Try(schema
      .getObjectType(validationType)
      .getFieldDefinitions
      .asScala).recover {
      case NonFatal(_) => throw MissingValueTypeInSchema(validationType, schema.getAllTypesAsList.asScala.map(_.getName))
    }
  }
}

object GraphQLValidator {
  sealed trait ValidationError extends IllegalStateException
  case class MissingFields(fields: Seq[String]) extends ValidationError {
    override def toString: String = fields.toString
  }
  case class MissingValueTypeInSchema(missingType: String, allSchemaTypes: Seq[String]) extends ValidationError {
    override def toString: String = s"There is an missing type $missingType in a schema: $allSchemaTypes"
  }
}