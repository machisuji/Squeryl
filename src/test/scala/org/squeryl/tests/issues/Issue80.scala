package org.squeryl.tests.issues

import org.scalatest.Spec
import org.scalatest.TestFailedException
import org.scalatest.matchers.ShouldMatchers
import org.squeryl.PrimitiveTypeMode._

class Issue80 extends Spec with ShouldMatchers with TestConnection {

  override def withFixture(test: NoArgTest) {
    try {
      using(session) {
        Population.create
        test()
      }
    } catch {
      case ex: TestFailedException => throw ex
      case ex: Throwable => ex.printStackTrace
    } finally {
      using(session) {
        Population.drop
      }
    }
  }

  describe("Schema") {
    it("should print the DDL") {
      Population.printDdl
    }
  }

  describe("CommonPerson") {
    it("should be able to have several addresses") {
      val person = new CommonPerson("Markus", "Kahl")
      transaction {
        val addr1 = new CommonsAddress("Berliner Str. 99", "54321")
        val addr2 = new CommonsAddress("Breite Straße 3", "12345")
        Population.commonPeople.insert(person)
        person.addresses.associate(addr1)
        person.addresses.associate(addr2)
        println("Address's person: " + addr1.person.statement)
      }
      val addresses = person.addresses.toList
      addresses should have size(2)
      addresses.foreach { addr =>
        val owner = addr.person.headOption.getOrElse(fail("No person associated with this address!"))
        owner should equal (person)
      }
      println("Person's addresses: " + person.addresses.statement)
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
  val postalCode: String,
  val id: Long = 0
) extends KeyedEntity[Long]

class Person(
  val firstName: String,
  val lastName: String
)

trait CommonName extends KeyedEntity[Long] { this: CommonPerson =>
  val id: Long = 0
  lazy val addresses: OneToMany[CommonsAddress] = Population.commonPersonToAddress.left(this)
}

class CommonPerson(
  firstName: String,
  lastName: String
) extends Person(firstName, lastName) with CommonName

class UniquePerson(
  firstName: String,
  lastName: String
) extends Person(firstName, lastName) with UniqueName

trait UniqueName extends KeyedEntity[CompositeKey2[String, String]] { this: UniquePerson =>
  val firstName: String
  val lastName: String
  def id = compositeKey(firstName, lastName)
  lazy val addresses: OneToMany[UniquesAddress] = Population.uniquePersonToAddress.left(this)
}

class CommonsAddress(
  street: String,
  postalCode: String,
  val personId: Long = 0
) extends Address(street, postalCode) {
  lazy val person: ManyToOne[CommonPerson] = Population.commonPersonToAddress.right(this)
}

class UniquesAddress(
  street: String,
  postalCode: String,
  val personId: CompositeKey2[String, String]
) extends Address(street, postalCode) {
  lazy val person: ManyToOne[UniquePerson] = Population.uniquePersonToAddress.right(this)
}

object Population extends Schema {

  val commonPeople = table[CommonPerson]
  val uniquePeople = table[UniquePerson]
  
  val commonsAddresses = table[CommonsAddress]
  val uniquesAddresses = table[UniquesAddress]
  
  val commonPersonToAddress = {
    val rel = oneToManyRelation(commonPeople, commonsAddresses)
    rel.via((person, address) => person.id === address.personId)
  }
  val uniquePersonToAddress = {
    val rel = oneToManyRelation(uniquePeople, uniquesAddresses)
    rel.via { (person, address) =>
      person.id === address.personId
    }
  }
  
  override def drop = super.drop
}