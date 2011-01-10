package org.squeryl.tests.issues

import org.specs._

import java.sql.{Connection, DriverManager}
import org.squeryl.{Session, SessionFactory}
import org.squeryl.adapters.H2Adapter

class Issue69 extends Specification with TestConnection {
  doBeforeSpec {
    org.squeryl.PrimitiveTypeMode.using(session) {
      try {
        Graph.create
        Graph.printDdl
      } catch {
        case ex: Throwable =>
          System.err.println(ex.getClass().getSimpleName() + ": " + ex.getMessage())
      }
    }
  }
  
  "A Node" should {
    "[Issue 69] not need to have a parent (nullable foreign key)." in {
      import org.squeryl.PrimitiveTypeMode._
      using(session) {
        val node = new Node("Root") // has no parent!
        from(Graph.nodes)(n => select(n)) must haveSize (0)
        transaction {
          Graph.nodes.insert(node)
        }
        from(Graph.nodes)(n => select(n)) must haveSize (1)
      }
    }
  }
}

import org.squeryl.{Schema, Table, KeyedEntity}
import org.squeryl.dsl.{OneToMany, ManyToOne}
import org.squeryl.PrimitiveTypeMode._

class Node(
  val name: String,
  val id: Long = 0,
  val parentId: Option[Long] = None
) extends KeyedEntity[Long] {
  def this() = this("", 0, Some(0L))
  def parent: ManyToOne[Node] = Graph.parentChildRelation.right(this)
  def children: OneToMany[Node] = Graph.parentChildRelation.left(this)
}

object Graph extends Schema {
  val nodes = table[Node]
  val parentChildRelation = {
    val rel = oneToManyRelation(nodes, nodes)
    rel.via((parent, child) => parent.id === child.parentId)
  }  
  override def drop = super.drop
}
