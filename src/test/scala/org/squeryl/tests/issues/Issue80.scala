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

class Address(
  val street: String,
  val postalCode: String
)
class Person(
  val firstName: String,
  val lastName: String
)

trait CommonName extends KeyedEntity[Long] { this: Person =>
  val id: Long = 0
  val addresses: OneToMany[CommonsAddress] = Population.commonPersonToAddress.left(this)
}
trait UniqueName extends KeyedEntity[CompositeKey2[String, String]] { this: Person =>
  val firstName: String
  val lastName: String
  def id = compositeKey(firstName, lastName)
  val addresses: OneToMany[UniquesAddress] = Population.uniquePersonToAddress.left(this)
}

class CommonsAddress(
  street: String,
  postalCode: String,
  val personId: Long
) extends Address(street, postalCode) {
  val person: ManyToOne[Person with CommonName] = Population.commonPersonToAddress.right(this)
}

class UniquesAddress(
  street: String,
  postalCode: String,
  val personId: CompositeKey2[String, String]
) extends Address(street, postalCode) {
  val person: ManyToOne[Person with UniqueName] = Population.uniquePersonToAddress.right(this)
}

object Population extends Schema {

  val commonPeople = table[Person with CommonName]
  val uniquePeople = table[Person with UniqueName]
  
  val commonsAddresses = table[CommonsAddress]
  val uniquesAddresses = table[UniquesAddress]
  
  val commonPersonToAddress = {
    val rel = oneToManyRelation(commonPeople, commonsAddresses)
    rel.via((person, address) => person.id === address.personId)
  }
  val uniquePersonToAddress = {
    val rel = oneToManyRelation(uniquePeople, uniquesAddresses)
    rel.via((person, address) => person.id === address.personId)
  }
  
  override def drop = super.drop
}
