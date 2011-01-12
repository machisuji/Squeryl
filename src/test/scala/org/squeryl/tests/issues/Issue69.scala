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
        val getNodes = from(Graph.nodes)(n => select(n))
        getNodes must haveSize (0)
        transaction {
          Graph.nodes.insert(node)
        }
        getNodes must haveSize (1)
        node.parent must haveSize (0)
        node.children must haveSize (0)
      }
    }
    
    "however, be able to have a parent." in {
      import org.squeryl.PrimitiveTypeMode._
      using(session) {
        val node = new Node("Adam")
        val getRoot = from(Graph.nodes)(n => where(n.name === "Root") select(n))
        val root = getRoot.head
        root.children must haveSize(0)
        transaction {
          Graph.nodes.insert(node)
          node.parent must haveSize (0)
          root.children.associate(node)
        }
        val children = getRoot.head.children.toList
        children must haveSize(1)
        children(0) mustEqual node
      }
    }
    
    "let me see ..." in {
      import org.squeryl.PrimitiveTypeMode._
      using(session) {
        val nodes = from(Graph.nodes)(select(_)).toList
        println("\tID\tNAME\tPARENT_ID")
        nodes.foreach { node =>
          print("\t" + node.id + "\t" + node.name + "\t")
          val parent = node.parent.headOption
          if (parent.isDefined) println(parent.get.id)
          else println("-")
        }
        nodes must haveSize(2)
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
