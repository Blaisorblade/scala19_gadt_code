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
  // Variables
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

  // A type of order-preserving embeddings.
  // Ope[G1, G2] proves that G1 can be projected to G2.
  enum Ope[G1, G2] {
    case Nil extends Ope[Unit, Unit]
    case Keep[A, G1, G2](o: Ope[G1, G2]) extends Ope[(A, G1), (A, G2)]
    case Drop[A, G1, G2](o: Ope[G1, G2]) extends Ope[(A, G1), G2]
  }
  import Ope._

  def evalOpe[G1, G2](o: Ope[G1, G2])(rho: G1): G2 = o match {
    case Nil => rho
    case k: Keep[a, g1, g2] =>
      val rho0: (a, g1) = rho
      (rho0._1, evalOpe(k.o)(rho0._2))
    case d: Drop[a, g1, g2] =>
      evalOpe(d.o)((rho: (a, g1))._2)
  }
  def assertType[X](x: X): Unit = {}

  // "Compose" a variable and an OPE.
  // - Var[G2, A] lets you project an A from G2;
  // - Ope[G1, G2] lets you project a G2 from G1
  // so we can project an A from G1.
  def weakVar[G1, G2, A](o: Ope[G1, G2])(x: Var[G2, A]): Var[G1, A] =
    o match {
      case n: Nil.type => x // dead branch
      case k: Keep[a, g1, g2] =>
        x match {
          case z: Z[_, _] => Z[g1, A]()
          case s: S[g, a0, b0] =>
            S[B = b0](weakVar(k.o)(s.x))
        }
      case d: Drop[b, g1, g2] =>
        // We have G2 = g2, G1 = (b, g1).
        // by induction we have Var[g1, A] — we can project an A from g1!
        assertType[Ope[g1, g2]](d.o)
        val x1 = weakVar(d.o)(x)
        assertType[Var[g1, A]](x1)
        // Goal: Var[G1, A] = Var[(b, g1), A].
        val res = S[B = b](x1)
        assertType[Var[(b, g1), A]](res)
        res
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

object stlc_deBruijn_encoded {
  object Version1 {
    trait Var[G, A]
    case class Z[G, A] extends Var[(A, G), A]
    case class S[G, A, B](x: Var[G, A]) extends Var[(B, G), A]

    def evalVar[G, A](x: Var[G, A])(rho: G): A = x match {
      case _: Z[g, a] => (rho: (a, g))._1
      case s: S[g, a, b] => evalVar(s.x)((rho: (b, g))._2)
    }
  }
  object Version2 {
    abstract class VarBase { x =>
      type G; type A
      // def `match`: [R] => (
      //   z: (Z & x.type) => R,
      //   s: (S & x.type) => R
      // ) => R
      def `match`[R]: (
        z: (Z & x.type) => R,
        s: (S & x.type) => R
      ) => R
    }
    type Var[g, a] = VarBase { type G = g; type A = a }

    abstract class Z extends VarBase {
      type GR
      type G = (A, GR)
      override def `match`[R] = (z, s) => z(this)
    }

    abstract class S extends VarBase {
      type GR
      type B
      type G = (B, GR)
      val x: Var[GR, A]
      override def `match`[R] = (z, s) => s(this)
    }

    def evalVar[G, A](x: Var[G, A])(rho: G): A =
      x.`match`[A](
        z => (rho: z.G)._1,
        s => evalVar(s.x)((rho: s.G)._2)
      )
  }
}
