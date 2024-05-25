package homework3

import homework3.io.IO

object UserRegistrationApp:
  val validPostalCodes: String => Boolean = Set("1000", "1164", "9000")
  val today = Date(2024, 1, 5)
  val register = UserRegistration.registerUser(validPostalCodes, today)(_)

  case class DateInput(year: String, month: String, day: String)

  private def prompt(promptMessage: String): IO[String] =
    for
      _ <- IO.println(promptMessage)
      input <- IO.readln
    yield input

  private def promptForBoolean(promptMessage: String): IO[Boolean] =
    for
      input <- prompt(promptMessage)
      inputBoolean <- input match
        case "y" => IO.of(true)
        case "n" => IO.of(false)
        case _ => IO.println("Please enter one of ['y', 'n']:") >> promptForBoolean(promptMessage)
    yield inputBoolean

  private def promptForBirthday(): IO[DateInput] =
    for
      _ <- IO.println("Enter your birthday:")
      birthYear <- prompt("year:")
      birthMonth <- prompt("month:")
      birthDay <- prompt("day:")
    yield DateInput(birthYear, birthMonth, birthDay)

  private def showUserRegistrationForm(): IO[RegistrationForm] =
    for
      name <- prompt("Enter your name:")
      email <- prompt("Enter your email:")
      password <- prompt("Enter your password:")
      passwordConfirmation <- prompt("Confirm your password:")
      DateInput(birthYear, birthMonth, birthDay) <- promptForBirthday()
      postalCode <- prompt("Enter your postal code (optional):")
    yield RegistrationForm(name, email, password, passwordConfirmation, birthYear, birthMonth, birthDay, postalCode)

  private def getDateErrorMessages(dateErrors: Chain[DateError]): List[String] =
    dateErrors.toList.map:
      case DateError.YearIsNotAnInteger(year) => s"Year ${year} is not an integer"
      case DateError.MonthIsNotAnInteger(month) => s"Month ${month} is not an integer"
      case DateError.DayIsNotAnInteger(day) => s"Day ${day} is not an integer"
      case DateError.MonthOutOfRange(month) => s"Month ${month} is out of range"
      case DateError.DayOutOfRange(day) => s"Day ${day} is out of range"
      case DateError.InvalidDate(year, month, day) => s"Date ${Date.toIsoString(year, month, day)} is invalid"

  private def getFormErrorMessages(errors: Chain[RegistrationFormError]): List[String] =
    errors.toList.flatMap:
      case RegistrationFormError.NameIsEmpty => List("Name is empty")
      case RegistrationFormError.InvalidEmail(email) => List(s"Provided email ${email} is not valid")
      case RegistrationFormError.PasswordTooShort =>
        List("Password is too short. It should be at least 8 symbols long")
      case RegistrationFormError.PasswordRequiresGreaterSymbolVariety =>
        List("Password should contain at least one letter, digit and a special symbol")
      case RegistrationFormError.PasswordsDoNotMatch => List("Passwords do not match")
      case RegistrationFormError.InvalidBirthdayDate(dateErrors) =>
        "Provided birthday date is invalid:" +: getDateErrorMessages(dateErrors)
      case RegistrationFormError.BirthdayDateIsInTheFuture(date) =>
        List(s"Birthday date ${Date.toIsoString(date)} is in the future")
      case RegistrationFormError.InvalidPostalCode(code) => List(s"Provided postal code '${code}' is invalid")

  private def showRegistrationErrors(errors: Chain[RegistrationFormError]): IO[Unit] =
    IO.println("The following errors have been found:") >>
      getFormErrorMessages(errors)
        .map(IO.println)
        .reduceLeft((combinedPrints, printIo) => combinedPrints >> printIo)

  private def registerUser(
    registrationForm: RegistrationForm,
    register: (RegistrationForm) => Validated[RegistrationFormError, User]
  ): IO[Unit] =
    register(registrationForm).fold(
      showRegistrationErrors,
      user => IO.println("User registered successfully") >> IO.println(user.toString)
    )

  val userRegistrationApp: IO[Unit] =
    for
      registrationForm <- showUserRegistrationForm()
      _ <- IO.println("")
      _ <- registerUser(registrationForm, register)
      _ <- IO.println("")
      shouldContinue <- promptForBoolean("Continue with another registration?")
      _ <- IO.println("")
      _ <- if shouldContinue then userRegistrationApp else IO.println("Have a great day!")
    yield ()

  def main(args: Array[String]): Unit = userRegistrationApp.unsafeRun()
