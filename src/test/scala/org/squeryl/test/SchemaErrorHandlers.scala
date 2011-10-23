package org.squeryl.test

import org.scalatest._
  import matchers.ShouldMatchers

import org.squeryl._
  import PrimitiveTypeMode._
  import framework.DBConnector
  import h2.H2_Connection

import java.sql.SQLException

case class Apple(val id: String) {
  def this() = this("")
  def kind = id
}

case class Orange(val id: String) {
  def this() = this("")
  def kind = id
}

object SubsetSchema extends Schema {
  val apples = table[Apple]
}

object SupersetSchema extends Schema {
  val apples = table[Apple]
  val oranges = table[Orange]
}

object FlexibleSupersetSchema extends Schema {
  val apples = table[Apple]
  val oranges = table[Orange]

  override val errorHandler = Some((e: SQLException, stmnt: String, session: Session) => {
    val exists = "(?i)table.*already\\s*exists".r
    (exists findFirstIn e.getMessage).map(_ => ()).getOrElse(throw e)
  })
}

trait SchemaErrorHandlers extends FunSuite with ShouldMatchers with BeforeAndAfterAll with DBConnector {

  override def beforeAll {
    SessionFactory.concreteFactory = connectToDb
  }

  test("SupersetSchema creation should fail if there already exists a subset.") {
    try {
      transaction(SubsetSchema.create)
      val thrown = evaluating {
        transaction(SupersetSchema.create)
      } should produce [RuntimeException]
      thrown.getMessage should include ("already exists")
    } finally {
      transaction {
        SubsetSchema.drop; SupersetSchema.drop
      }
    }
  }

  test("SupersetSchema creation should work even if there already exists a subset when using a graceful error handler") {
    try {
      transaction(SubsetSchema.create)
      try {
        transaction(FlexibleSupersetSchema.create)
      } catch {
        case e: RuntimeException => fail("Should not produce a RuntimeException")
      }
      try {
        transaction {
          from(FlexibleSupersetSchema.oranges)(o => select(o)).toList
        }
      } catch {
        case e: RuntimeException => fail("Should not produce a RuntimeException")
      }
    } finally {
      transaction {
        SubsetSchema.drop; FlexibleSupersetSchema.drop
      }
    }
  }
}

class TestErrorHandlersH2 extends SchemaErrorHandlers with H2_Connection