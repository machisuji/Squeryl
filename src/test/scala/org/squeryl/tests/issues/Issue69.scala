package org.squeryl.tests.issues

import org.specs._

import java.sql.{Connection, DriverManager}
import org.squeryl.{Session, SessionFactory}
import org.squeryl.adapters.H2Adapter
import org.squeryl.PrimitiveTypeMode

class Issue69 extends Specification with TestConnection {
  
  def createSchema() {
    PrimitiveTypeMode.using(session) {
      try {
        Graph.create
      } catch {
        case ex: Throwable =>
          System.err.println(ex.getClass().getSimpleName() + ": " + ex.getMessage())
      }
    }
  }
  
  "Squeryl" should {
    
    doBefore {
      import PrimitiveTypeMode._
      createSchema()
      using(session) {
        val root = new Node("Root")
        transaction {
          Graph.nodes.insert(root)
        }
        val adam = new Node("Adam")
        transaction {
          Graph.nodes.insert(adam)
          root.children.associate(adam)
        }
      }
    }
    
    doAfter {
      PrimitiveTypeMode.using(session) {
        Graph.drop
      }
    }
  
    "support (optional) foreign keys." in {
      import PrimitiveTypeMode._
      using(session) {
        val getRoot = from(Graph.nodes)(n => where(n.name === "Root") select(n))
        val root = getRoot.headOption.getOrElse(fail("Root not found"))
        root.parent must beEmpty
        
        val getAdam = from(Graph.nodes)(n => where(n.name === "Adam") select(n))
        val adam = getAdam.headOption.getOrElse(fail("Adam not found"))
        adam.parent must beEmpty.not
        adam.parent.head mustEqual root
        root.children must contain(adam)
      }
    }
    
    "allow foreign keys to be set to None." in {
      import PrimitiveTypeMode._
      using(session) {
        val (adam, parent) = getAdamAndHisParent
        adam.parent must beEmpty.not // in one moment it still has a parent
        transaction {
          adam.parentId = None
          Graph.nodes.update(adam)
        }
        adam.parent must beEmpty // and in the next ... no more!
      }
    }
    
    "allow deletion of no longer referenced rows." in {
      import PrimitiveTypeMode._
      using(session) {
        val (adam, parent) = getAdamAndHisParent
        def illegalAction = {
          transaction {
            adam.parent.delete
          }
        }
        illegalAction must throwAn[Exception] // still referenced, do not allow deletion
        transaction {
          adam.parentId = None
          Graph.nodes.update(adam)
        }
        val getNodes = from(Graph.nodes)(select(_))
        getNodes must haveSize(2)
        transaction {
          Graph.nodes.deleteWhere(n => n.name === "Root")
        }
        getNodes must haveSize(1) // since no more referenced, deletion should be allowed
      }
    }
    
    def getAdamAndHisParent: (Node, Node) = {
      import PrimitiveTypeMode._
      val getAdam = from(Graph.nodes)(n => where(n.name === "Adam") select(n))
      val adam = getAdam.headOption.getOrElse(fail("Could not find Adam!"))
      val parent = adam.parent.headOption.getOrElse(fail("Adam has no parent!"))
      (adam, parent)
    }
    
    "let me see ..." in {
      import PrimitiveTypeMode._
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
  var parentId: Option[Long] = None
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
