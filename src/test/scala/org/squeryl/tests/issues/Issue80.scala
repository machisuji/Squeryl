package org.squeryl.tests.issues

import org.specs._

import java.sql.{Connection, DriverManager}
import org.squeryl.{Session, SessionFactory}
import org.squeryl.adapters.H2Adapter

class Issue80 extends Specification with TestConnection {
  doBeforeSpec {
    org.squeryl.PrimitiveTypeMode.using(session) {
      try {
        Population.create
        Population.printDdl
      } catch {
        case ex: Throwable =>
          System.err.println(ex.getClass().getSimpleName() + ": " + ex.getMessage())
      }
    }
  }
  
  "Common people" should {
    "Foreign keys should be nullable (issue 60)" in {
      import org.squeryl.PrimitiveTypeMode._
      using(session) {
        val people = from(Population.commonPeople)(p => select(p))
        val guy = new CommonsChild("Hans", "Wurst")
        people must haveSize (0)
        transaction {
          Population.commonPeople.insert(guy)
        }
        people must haveSize (1)
      }
    }
  }
  
  "Normal keys" should {
    "be usable in binding expressions for relations" in {
      // pending
    }
  }
  "Composite keys" should {
    "be usable in binding expressions for relations" in {
      // pending
    }
  }
}

import org.squeryl.Schema
import org.squeryl.Table
import org.squeryl.KeyedEntity
import org.squeryl.dsl.CompositeKey2
import org.squeryl.dsl.ManyToOne
import org.squeryl.dsl.OneToMany
import org.squeryl.dsl.OneToManyRelation
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.annotations._

class Person(
  val firstName: String,
  val lastName: String
)
trait Identity
trait CommonName extends Identity with KeyedEntity[Long] {
  val id: Long = 0
}
trait UniqueName extends Identity with KeyedEntity[CompositeKey2[String, String]] {
  val firstName: String
  val lastName: String
  def id = compositeKey(firstName, lastName)
}

class CommonsChild(
  firstName: String,
  lastName: String,
  val parentId: Option[Long] = None
) extends Person(firstName, lastName) with CommonName with CommonChildren {
  lazy val parent: Option[ManyToOne[CommonsChild]] =
    if (parentId.isDefined) Some(Population.commonPeoplesChildren.right(this))
    else noParent
  def this() = this(null, null, Some(0L))
  def this(firstName: String, lastName: String, parent: CommonsChild) = {
    this(firstName, lastName)
    parent.children.assign(this)
  }
  private def noParent =
    if (firstName == null && lastName == null) {
      println("Returning Some with null value")
      Some[ManyToOne[CommonsChild]](null)
    } else None
}

/*class UniquesChild(
  firstName: String,
  lastName: String,
  val parentId: CompositeKey2[String, String] = null
) extends Person(firstName, lastName) with UniqueName with UniqueChildren {
  lazy val parent: ManyToOne[UniquesChild] = Population.uniquePeoplesChildren.right(this)
  def this() = this("", "", new CompositeKey2[String, String]("", ""))
  def this(firstName: String, lastName: String, parent: UniquesChild) = {
    this(firstName, lastName)
    parent.children.assign(this)
  }
}*/

trait CommonChildren { this: CommonsChild =>
  lazy val children: OneToMany[CommonsChild] = Population.commonPeoplesChildren.left(this)
}
/*trait UniqueChildren { this: UniquesChild =>
  lazy val children: OneToMany[UniquesChild] = Population.uniquePeoplesChildren.left(this)
}*/

// A person of this population does not need a partner to reproduce itself.
object Population extends Schema {
  println("What the!?")
  var commonPeople: Table[CommonsChild] = null
  try {
    commonPeople = table[CommonsChild]
  } catch {
    case ex: Throwable => ex.printStackTrace
  }
  println("That !?")
  
  val commonPeoplesChildren = {
    println("Initialising common people's children")
    val rel = oneToManyRelation(commonPeople, commonPeople)
    println("Done: " + rel)
    rel.via { (parent, child) =>
      parent.id === child.parentId
    }
  }

  /*@TODO Make the following compile:
  val uniquePeople = table[UniquesChild]

  val uniquePeoplesChildren: OneToManyRelation[UniquesChild, UniquesChild] =
    oneToManyRelation(uniquePeople, uniquePeople).via { (parent, child) =>
      parent.id === child.parentId
    }
  */
  
  override def drop = super.drop
}
