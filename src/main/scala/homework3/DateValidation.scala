package homework3

import homework3.DateError.InvalidDate
import homework3.Validated.toValidated

enum DateError:
  case YearIsNotAnInteger(year: String)
  case MonthIsNotAnInteger(month: String)
  case DayIsNotAnInteger(day: String)
  case MonthOutOfRange(month: Int)
  case DayOutOfRange(day: Int)
  case InvalidDate(year: Int, month: Int, day: Int)

object DateValidation:
  val MonthRange: Range.Inclusive = 1 to 12
  val DayRange: Range.Inclusive = 1 to 31

  def validateDate(year: String, month: String, day: String): Validated[DateError, Date] =
    val validatedYear = year.toIntOption.toValidated(DateError.YearIsNotAnInteger(year))

    val validatedMonth =
      for
        monthInt <- month.toIntOption.toValidated(DateError.MonthIsNotAnInteger(month))
        _ <- Validated.onCondition(monthInt, MonthRange.contains, DateError.MonthOutOfRange(monthInt))
      yield monthInt

    val validatedDay =
      for
        dayInt <- day.toIntOption.toValidated(DateError.DayIsNotAnInteger(day))
        _ <- Validated.onCondition(dayInt, DayRange.contains, DateError.DayOutOfRange(dayInt))
      yield dayInt

    (
      validatedYear,
      validatedMonth,
      validatedDay
    ).zipN.flatMap:
      case (y, m, d) =>
        Date.applyOption(y, m, d).toValidated(InvalidDate(y, m, d))
