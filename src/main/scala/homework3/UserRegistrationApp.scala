package homework3

import homework3.io.IO

object UserRegistrationApp:
  val validPostalCodes: String => Boolean = Set("1000", "1164", "9000")
  val today = Date(2024, 1, 5)
  val register = UserRegistration.registerUser(validPostalCodes, today)(_)

  val userRegistrationApp: IO[Unit] = ???

  def main(args: Array[String]): Unit = userRegistrationApp.unsafeRun()
