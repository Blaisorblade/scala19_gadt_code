import scala.language.implicitConversions

// Lambda calculus interpreters.
// Version 1, using HOAS (Higher-Order Abstract Syntax), from Figure 1.

object stlc_hoas {
  enum Expr[A] {
    case Lit(n: Int) extends Expr[Int]
    case Plus(lhs: Expr[Int], rhs: Expr[Int])
            extends Expr[Int]
    case Var(a: A) extends Expr[A]
    case Fun[A,B](fun: Expr[A] => Expr[B])
            extends Expr[A => B]
    case App[A,B](fun: Expr[A => B], arg: Expr[A])
            extends Expr[B]
  }
  import Expr._
  def eval[A](e: Expr[A]): A = e match {
    case Lit(n) => n
    case Plus(a,b) => eval(a) + eval(b)
    case Var(x) => x
    case f: Fun[a,b] => (x: a) => eval(f.fun(Var(x)))
    case App(fun,arg) => eval(fun)(eval(arg))
  }
}

// Version 2, using typed de Bruijn indices

// This is inspired by the encoding that is common in Agda, and by the GADT-less encoding
// in Haskell by Oleg in http://okmij.org/ftp/tagless-final/course/TTFdB.hs, in particular:

// class Symantics repr where
//     int :: Int -> repr h Int                -- int literal
//     add :: repr h Int -> repr h Int -> repr h Int
//     z   :: repr (a,h) a                 -- variables: z and s ... (s z)
//     s   :: repr h a -> repr (any,h) a
//     lam :: repr (a,h) b  -> repr h (a->b)
//     app :: repr h (a->b) -> repr h a -> repr h b

// In particular, we follow Oleg in encoding contexts directly as the type of
// corresponding environments.

// However, we separate types for variables and terms (like in Agda, but unlike
// in the above encoding).

object stlc_deBruijn {
  // Variablea
  enum Var[G, A] {
    case Z[G, A] extends Var[(A, G), A]
    case S[G, A, B](x: Var[G, A]) extends Var[(B, G), A]
  }
  import Var._

  // None of the type ascriptions below can be omitted.
  def evalVar[G, A](x: Var[G, A])(rho: G): A = x match {
    case _: Z[g, a] =>
      (rho: (a, g))._1
    case s: S[g, a, b] =>
      evalVar(s.x)((rho: (b, g))._2)
  }

  enum Expr[G, A] {
    case Lit[G](n: Int) extends Expr[G, Int]
    case V[G, A](x: Var[G, A]) extends Expr[G, A]
    case App[G, A, B](f: Expr[G, A => B], a: Expr[G, A]) extends Expr[G, B]
    case Fun[G, A, B](body: Expr[(A, G), B]) extends Expr[G, A => B]
  }
  import Expr._
  def eval[A, G](e: Expr[G, A])(rho: G): A = e match {
    // case Lit(n) => n // spurious unchecked warning
    case l: Lit[_] => l.n

    case V(x) => evalVar(x)(rho)
    case App(f, a) => eval(f)(rho)(eval(a)(rho))
    case f: Fun[g, a, b] =>
      (x: a) => eval(f.body)(x, rho)
  }
}
