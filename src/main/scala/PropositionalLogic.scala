object PropositionalLogic {

  type Bottom = Nothing
  type Top = Unit
  type /\[A, B] = (A, B)
  type \/[A, B] = Either[A, B]

  type Not[A] = A => Bottom
  type <=>[A, B] = (A => B) /\ (B => A)

  opaque type Prop[A] = A
  type Premise[A] = Prop[A]
  type Conclusion[A] = Prop[A]

  def example1[A, B]: Premise[A] => Premise[B] => Conclusion[A /\ B] =
    a => b => (a, b)

  def example2[A, B]: (Premise[A => B], Premise[A]) => Conclusion[B] = {
    case (f, a) => f(a)
  }

  def example3[A]: Premise[Bottom] => Conclusion[A] =
    bottom => example3(bottom) // This is OK, since we cannot construct bottom

  def exercise1[A, B, C]
      : (Premise[A => B], Premise[B => C]) => Conclusion[A => C] = {
    case (ab, bc) => a => bc(ab(a))
  }

  def exercise2[A, B, C]
      : (Premise[A \/ B], Premise[A => C], Premise[B => C]) => Conclusion[C] = {
    case (Left(a), ac, _)  => ac(a)
    case (Right(b), _, bc) => bc(b)
  }

  trait Decidable[A] {
    def lem: A \/ Not[A]
  }

  def exercise7[P, Q](using
      dp: Decidable[P]
  ): Conclusion[(P => Q) <=> (Not[P] \/ Q)] =
    dp.lem match
      case Left(p) =>
        (
          pq => Right(pq(p)),
          {
            case Left(np) => example3(np(p))
            case Right(q) => _ => q
          }
        )
      case Right(np) => (_ => Left(np), _ => p => example3(np(p)))

}
