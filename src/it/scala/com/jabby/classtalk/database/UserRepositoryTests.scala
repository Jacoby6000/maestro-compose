package com.jabby.classtalk.database

import doobie.util.iolite.IOLite
import doobie.specs2.imports._
import doobie.imports._
import org.specs2.mutable.Specification

class UserRepositoryTests extends Specification with AnalysisSpec {

  val transactor = DriverManagerTransactor[IOLite](
    "org.postgresql.Driver", "jdbc:postgresql:classtalk", "application", ""
  )

  val repository = new DoobieUserRepository

  check(repository.getAllUsersQuery)
}


