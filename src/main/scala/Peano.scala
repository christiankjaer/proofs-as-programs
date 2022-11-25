object Peano {

  sealed trait Nat
  sealed trait Z extends Nat
  sealed trait S[N <: Nat] extends Nat

  type +[A <: Nat, B <: Nat] = (A, B) match {
    case (Z, n)    => n
    case (S[n], m) => S[n + m]
  }

  sealed trait ==[A, B]
  case class Refl[A]() extends ==[A, A]

  def qed[A](a: A, b: A): A == A = Refl()

  def symmetric[A, B]: A == B => B == A = { case Refl() =>
    Refl()
  }

  def congruent[A, B, F[_]]: A == B => F[A] == F[B] = { case Refl() =>
    Refl()
  }

  def transitive[A, B, C]: A == B => B == C => A == C =
    ab =>
      bc =>
        (ab, bc) match {
          case (Refl(), Refl()) => Refl()
        }


  def zero[N <: Nat](n: N): N + Z == N = n match {
    case m: Z => ???
    case m: S[n] => ???
  }

  type Two = S[S[Z]]
  type Two_ = S[Z] + S[Z]

  // Prove that 1+1=2
  val foo: Two == Two_ = Refl()

}
