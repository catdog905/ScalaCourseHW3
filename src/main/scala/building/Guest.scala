package building

sealed trait AgeError
case object NonPositiveAge extends AgeError

sealed trait Guest {
  def age: Int
}

case class Man private (age: Int) extends Guest

object Man {
  def apply(age: Int): Either[AgeError, Man] = age match {
    case age if age >= 1 => Right(new Man(age))
    case _               => Left(NonPositiveAge)
  }
}
case class Woman private (age: Int) extends Guest

object Woman {
  def apply(age: Int): Either[AgeError, Woman] = age match {
    case age if age >= 1 => Right(new Woman(age))
    case _               => Left(NonPositiveAge)
  }
}
