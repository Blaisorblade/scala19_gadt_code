object openGADTs {
  class Source {
    trait Expr[A]
    case class Lit(n: Int) extends Expr[Int]
    case class Plus(lhs: Expr[Int], rhs: Expr[Int]) extends Expr[Int]
    case class Var[A](a: A) extends Expr[A]
    case class Fun[A, B](fun: Expr[A] => Expr[B]) extends Expr[A => B]
    case class App[A, B](fun: Expr[A => B], arg: Expr[A]) extends Expr[B]

    def eval[A](e: Expr[A]): A = e match {
      case Lit(n) => n
      case Plus(lhs, rhs) => eval(lhs) + eval(rhs)
      case Var(x) => x
      case f: Fun[a,b] =>
        (x: a) => eval(f.fun(Var(x)))
      case App(fun,arg) => eval(fun)(eval(arg))
    }
  }

  class Encoded {
    type Expr[+a] = ExprBase { type A <: a }
    abstract class ExprBase { s =>
      type A
    }

    abstract class Lit extends ExprBase { s =>
      type A = Int
      val n: A
    }

    abstract class Plus extends ExprBase { s =>
      type A = Int
      val lhs: Expr[A]
      val rhs: Expr[A]
    }

    abstract class Var extends ExprBase { s =>
      val a: A
    }

    abstract class Fun extends ExprBase { s =>
      type B
      type C
      type A = B => C
      val fun: Expr[B] => Expr[C]
    }

    abstract class App extends ExprBase { s =>
      type B
      val fun: Expr[B => A]
      val arg: Expr[B]
    }

    def eval[A](e: Expr[A]): A = e match {
      case l: (Lit & e.type) => l.n: l.A
      case v: (Var & e.type) => v.a: v.A
      case p: (Plus & e.type) =>
        eval(p.lhs) + eval(p.rhs): p.A
      case a: (App & e.type) =>
        eval(a.fun)(eval(a.arg)): a.A
      case f: (Fun & e.type) =>
        ((x: f.B) =>
          eval(f.fun(new Var {type A = f.B; val a = x }))
        ): f.A
    }
  }

  class SourceExtend extends Source {
    case class Minus(lhs: Expr[Int], rhs: Expr[Int]) extends Expr[Int]
    override def eval[A](e: Expr[A]): A = e match {
      case Minus(lhs, rhs) =>
        eval(lhs) - eval(rhs)
      case _ =>
        super.eval(e)
    }
  }

  class EncodedExtends extends Encoded {
    abstract class Minus extends ExprBase { s =>
      type A = Int
      val lhs: Expr[A]
      val rhs: Expr[A]
    }
    override def eval[A](e: Expr[A]): A = e match {
      case m: (Minus & e.type) =>
        eval(m.lhs) - eval(m.rhs): m.A
      case _ =>
        super.eval(e): e.A
    }
  }
}
