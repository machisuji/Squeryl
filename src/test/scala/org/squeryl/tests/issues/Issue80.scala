package org.squeryl.tests.issues

import org.scalatest.Spec
import org.scalatest.TestFailedException
import org.scalatest.matchers.ShouldMatchers
import org.squeryl.PrimitiveTypeMode._

class Issue80 extends Spec with ShouldMatchers with TestConnection {

  def withSchema(schema: VolatileSchema)(block: => Unit) {
    try {
      using(session) {
        schema.create
        block
      }
    } catch {
      case ex: TestFailedException => throw ex
      case ex: Throwable => ex.printStackTrace
    } finally {
      using(session) {
        schema.drop
      }
    }
  }

  describe("As foreign keys the following should work") {
    it("Long") {
      withSchema(LongSchema) {
        val person = new CommonPerson("Markus", "Kahl")
        transaction {
          val addr1 = new CommonsAddress("Berliner Str. 99", "54321")
          val addr2 = new CommonsAddress("Breite Straße 3", "12345")
          LongSchema.commonPeople.insert(person)
          person.addresses.associate(addr1)
          person.addresses.associate(addr2)
        }
        val addresses = person.addresses.toList
        addresses should have size(2)
        addresses.foreach { addr =>
          val owner = addr.person.headOption.getOrElse(fail("No person associated with this address!"))
          owner should equal (person)
        }
      }
    }
    it("String") {
      withSchema(StringSchema) {
        val sol = new SolarSystem("Sol")
        transaction {
          StringSchema.solarSystems.insert(sol)
          val earth = new Planet("Earth", 12742, sol.id)
          val venus = new Planet("Venus", 12100, sol.id)
          StringSchema.planets.insert(earth :: venus :: Nil)
        }
        val planets = sol.planets.toList
        planets should have size(2)
        planets.foreach { planet =>
          val solarSystem = planet.solarSystem.headOption.getOrElse(fail("No solar system associated!"))
          solarSystem should equal (sol)
        }
      }
    }
    it("CompositeKey") {
      withSchema(CompositeSchema) {
        val person = new UniquePerson("Hans", "Wurst")
        transaction {
          CompositeSchema.uniquePeople.insert(person)
          val addr1 = new UniquesAddress("Wiener Straße 8", "54321", person.id)
          val addr2 = new UniquesAddress("Thüringer Allee 217", "12345", person.id)
          CompositeSchema.uniquesAddresses.insert(addr1 :: addr2 :: Nil)
        }
        val addresses = person.addresses.toList
        addresses should have size(2)
        addresses.foreach { addr =>
          val owner = addr.person.headOption.getOrElse(fail("No person associated with this address!"))
          owner should equal (person)
        }
      }
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
  lazy val addresses: OneToMany[CommonsAddress] = LongSchema.commonPersonToAddress.left(this)
}

class CommonPerson(
  firstName: String,
  lastName: String
) extends Person(firstName, lastName) with CommonName

class UniquePerson(
  firstName: String,
  lastName: String
) extends Person(firstName, lastName) with UniqueName { def foobar = "" }

trait UniqueName extends KeyedEntity[CompositeKey2[String, String]] { this: UniquePerson =>
  val firstName: String
  val lastName: String
  def id = compositeKey(firstName, lastName)
  lazy val addresses: OneToMany[UniquesAddress] = CompositeSchema.uniquePersonToAddress.left(this)
}

class CommonsAddress(
  street: String,
  postalCode: String,
  val personId: Long = 0
) extends Address(street, postalCode) {
  lazy val person: ManyToOne[CommonPerson] = LongSchema.commonPersonToAddress.right(this)
}

class UniquesAddress(
  street: String,
  postalCode: String,
  val personId: CompositeKey2[String, String] = compositeKey("foo", "bar")
) extends Address(street, postalCode) {
  lazy val person: ManyToOne[UniquePerson] = CompositeSchema.uniquePersonToAddress.right(this)
  def foobar = "addr"
}

class Planet(
  val id: String,
  val diameter: Long,
  val solarSystemId: String
) extends KeyedEntity[String] {
  lazy val solarSystem: ManyToOne[SolarSystem] = StringSchema.solarSystemToPlanets.right(this)
}

class SolarSystem(
  val id: String
) extends KeyedEntity[String] {
  lazy val planets: OneToMany[Planet] = StringSchema.solarSystemToPlanets.left(this)
}

class VolatileSchema extends Schema {
  override def drop = super.drop
}

object LongSchema extends VolatileSchema {
  val commonPeople = table[CommonPerson]
  val commonsAddresses = table[CommonsAddress]

  val commonPersonToAddress = {
    val rel = oneToManyRelation(commonPeople, commonsAddresses)
    rel.via((person, address) => person.id === address.personId)
  }
}

object StringSchema extends VolatileSchema {
  val planets = table[Planet]
  val solarSystems = table[SolarSystem]

  val solarSystemToPlanets = {
    val rel = oneToManyRelation(solarSystems, planets)
    rel.via((system, planet) => system.id === planet.solarSystemId)
  }
}

object CompositeSchema extends VolatileSchema {
  val uniquePeople = table[UniquePerson]
  val uniquesAddresses = table[UniquesAddress]

  val uniquePersonToAddress = {
    val rel = oneToManyRelation(uniquePeople, uniquesAddresses)
    rel.via { (person, address) =>
      println("person.id -> " + person.id)
      println("address.personId -> " + address.personId)
      person.id === address.personId
    }
  }
}