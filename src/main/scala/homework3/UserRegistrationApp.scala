package homework3

import homework3.io.IO

object UserRegistrationApp:
  import PromptUtils.*

  val validPostalCodes: String => Boolean = Set("1000", "1164", "9000")
  val today: Date = Date(2024, 1, 5)
  val register = UserRegistration.registerUser(validPostalCodes, today)(_)

  val userRegistrationApp: IO[Unit] =
    for
      registrationForm <- inputRegistration
      userValidation = register(registrationForm)
      _ <- outputRegistrationResult(userValidation)
      shouldContinue <- promptForBoolean("Continue with another registration?")
      _ <- if shouldContinue then userRegistrationApp else IO.println("Have a great day :)!")
    yield ()

  def inputRegistration: IO[RegistrationForm] =
    for
      name <- promptForInput("Enter your name:")
      email <- promptForInput("Enter your email:")
      password <- promptForInput("Enter your password:")
      passwordConfirmation <- promptForInput("Confirm your password:")
      DateInput(birthYear, birthMonth, birthDay) <- promptForDate("Enter your birthday:")
      postalCode <- promptForInput("Enter your postal code (optional):")
    yield RegistrationForm(name, email, password, passwordConfirmation, birthYear, birthMonth, birthDay, postalCode)

  def outputRegistrationResult(userValidation: Validated[RegistrationFormError, User]): IO[Unit] =
    userValidation.fold(
      errors =>
        errors
          .map(registrationFormErrorToDescription)
          .foldLeft(IO.println("The following errors have been found:"))(_ >> IO.println(_)),
      user => IO.println("User registered successfully") >> IO.println(user.toString)
    )

  def registrationFormErrorToDescription(registrationFormError: RegistrationFormError): String =
    registrationFormError match
      case RegistrationFormError.NameIsEmpty => "Name is empty"
      case RegistrationFormError.InvalidEmail(email) => s"Provided email $email is not valid"
      case RegistrationFormError.PasswordTooShort =>
        "Password is too short. It should be at least 8 symbols long"
      case RegistrationFormError.PasswordRequiresGreaterSymbolVariety =>
        "Password should contain at least one letter, digit and a special symbol"
      case RegistrationFormError.PasswordsDoNotMatch => "Passwords do not match"
      case RegistrationFormError.InvalidBirthdayDate(dateErrors) =>
        ("Provided birthday date is invalid:" :: dateErrors.toList.map(dateErrorToDescription)).mkString("\n")
      case RegistrationFormError.BirthdayDateIsInTheFuture(date) =>
        s"Birthday date $date is in the future"
      case RegistrationFormError.InvalidPostalCode(code) => s"Provided postal code '$code' is invalid"

  def dateErrorToDescription(dateError: DateError): String =
    dateError match
      case DateError.YearIsNotAnInteger(year) => s"Year $year is not an integer"
      case DateError.MonthIsNotAnInteger(month) => s"Month $month is not an integer"
      case DateError.DayIsNotAnInteger(day) => s"Day $day is not an integer"
      case DateError.MonthOutOfRange(month) => s"Month $month is out of range"
      case DateError.DayOutOfRange(day) => s"Day $day is out of range"
      case DateError.InvalidDate(year, month, day) => s"Date $year-$month-$day is invalid"

  def main(args: Array[String]): Unit = userRegistrationApp.unsafeRun()

object PromptUtils:
  case class DateInput(year: String, month: String, day: String)

  def promptForInput(prompt: String): IO[String] =
    for
      _ <- IO.println(prompt)
      input <- IO.readln
    yield input

  def promptForBoolean(prompt: String): IO[Boolean] = promptForInput(prompt).flatMap:
    case "true" | "yes" | "t" | "y" => IO.of(true)
    case "false" | "no" | "f" | "n" => IO.of(false)
    case _ => IO.println("Please enter 'yes' or 'no'") >> promptForBoolean(prompt)

  def promptForDate(prompt: String): IO[DateInput] =
    for
      _ <- IO.println(prompt)
      year <- promptForInput("year:")
      month <- promptForInput("month:")
      day <- promptForInput("day:")
    yield DateInput(year, month, day)
