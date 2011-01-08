package org.squeryl.tests.issues

import org.specs._

import java.sql.{Connection, DriverManager}
import org.squeryl.{Session, SessionFactory}
import org.squeryl.adapters.H2Adapter

class Issue80 extends Specification {
  val session = createTestConnection
  
  initSessionFactory(session)
  org.squeryl.PrimitiveTypeMode.using(session) {
    Population.create
    Population.printDdl
  }
  
  "Common people" should {
    "Foreign keys should be nullable (issue 60)" in {
      import org.squeryl.PrimitiveTypeMode._
      using(session) {
        val people = from(Population.commonPeople)(p => select(p))
        val guy = new PrimeChild
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
  
  def createTestConnection = {
    Class.forName("org.h2.Driver");
    Session.create(
      java.sql.DriverManager.getConnection("jdbc:h2:mem:test", "sa", ""),
      new H2Adapter
    )
  }
  def initSessionFactory(session: Session) {
    SessionFactory.concreteFactory = Some(() => session)
  }
}

import org.squeryl.Schema
import org.squeryl.KeyedEntity
import org.squeryl.dsl.CompositeKey2
import org.squeryl.dsl.ManyToOne
import org.squeryl.dsl.OneToMany
import org.squeryl.dsl.OneToManyRelation
import org.squeryl.PrimitiveTypeMode._

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
  val parentId: Long = 0
) extends Person(firstName, lastName) with CommonName with CommonChildren {
  lazy val parent: ManyToOne[CommonsChild] =
    Population.commonPeoplesChildren.right(this)
  def this() = this("", "", 0)
  def this(firstName: String, lastName: String, parent: CommonsChild) = {
    this(firstName, lastName)
    parent.children.assign(this)
  }
}

class PrimeChild(firstName: String, lastName: String, parentId: Long)
    extends CommonsChild(firstName, lastName, parentId) {
  def this() = {
    this("Prime", "Child", 0)
    children.assign(this)
  }
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
  val commonPeople = table[CommonsChild]
  //val uniquePeople = table[UniquesChild]
  
  val commonPeoplesChildren =
    oneToManyRelation(commonPeople, commonPeople).via { (parent, child) =>
      parent.id === child.parentId
    }

  /*@TODO Make the following compile:
  val uniquePeoplesChildren: OneToManyRelation[UniquesChild, UniquesChild] =
    oneToManyRelation(uniquePeople, uniquePeople).via { (parent, child) =>
      parent.id === child.parentId
    }
  */
  
  override def drop = super.drop
}
