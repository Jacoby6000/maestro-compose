package com.jabby.classtalk.application.services

import org.specs2._
import com.jabby.classtalk.database.UserRepository
import com.jabby.classtalk.database.UserRepository._
import com.jabby.classtalk.data._
import scalaz._, Scalaz._
import java.util.UUID
import java.time.Instant

class UserServiceTest extends Specification { def is = s2"""
  A User Service
    getUsers - Should properly convert List[UserNoJoin] to List[MinifiedUser] $getUsersTest

  """

  val dummyUsers = List(
    UserNoJoin(DatabaseId(UUID.randomUUID()), FirstName("John"), LastName("Doe"), None, Some(Instant.now())),
    UserNoJoin(DatabaseId(UUID.randomUUID()), FirstName("Jane"), LastName("Shmo"), None, Some(Instant.now()))
  )

  class DummyUserRepository extends UserRepository[Id] {
    def getAllUsers: Id[List[UserNoJoin]] = dummyUsers
  }

  val userServiceTransform: Id ~> Id =
    new (Id ~> Id) {
      def apply[A](id: Id[A]): Id[A] = id
    }


  val userService = new UserService[Id, Id](new DummyUserRepository, userServiceTransform)

  lazy val getUsersTest = {
    val result = userService.getUsers
    (result zip dummyUsers).map { case (user, dummyUser) =>
      user.id === dummyUser.id and
      user.firstName === dummyUser.firstName and
      user.lastName === dummyUser.lastName and
      user.lastActive === dummyUser.lastActive and
      user.profileImageLink === dummyUser.profileImageLink
    }
  }
}
