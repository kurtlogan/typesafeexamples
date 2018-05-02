package example.TotalAndSurjective

final class UppercaseString private (val underlying: String) extends AnyVal

object UppercaseString {
  def apply(str: String) = new UppercaseString(str.toUpperCase)
}

final class NonEmptyList[A] private (val underlying: List[A]) extends AnyVal

object NonEmptyList {
  def apply[A](list: List[A]): Option[NonEmptyList[A]] = list match {
    case List() => None
    case xs => Some(new NonEmptyList(xs))
  }
}

final class NaturalNumber private (val underlying: Int) extends AnyVal

object NaturalNumber {
  def apply(number: Int): Option[NaturalNumber] = number match {
    case i if i <= 0 => None
    case i => Some(new NaturalNumber(i))
  }
}