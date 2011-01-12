package org.squeryl.tests.issues

import org.specs._

import java.sql.{Connection, DriverManager}
import org.squeryl.{Session, SessionFactory}
import org.squeryl.adapters.H2Adapter

trait TestConnection {
  initSessionFactory()
  val session = SessionFactory.newSession
  
  def createTestConnection = {
    Class.forName("org.h2.Driver");
    Session.create(
      java.sql.DriverManager.getConnection("jdbc:h2:mem:test", "sa", ""),
      new H2Adapter
    )
  }
  def initSessionFactory() {
    SessionFactory.concreteFactory = Some(() => createTestConnection)
  }
}
