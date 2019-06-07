// object Test {
//   abstract class ExprBase { s =>
//     type A
//   }

//   abstract class Lit extends ExprBase { s =>
//     type A = Int
//     val n: A
//   }

//   abstract class LitU extends ExprBase { s =>
//     type A <: Int
//     val n: A
//   }

//   abstract class LitL extends ExprBase { s =>
//     type A <: Int
//     val n: A
//   }

//   def castTest1(e1: ExprBase)(e2: e1.type)(x: e1.A): e2.A = x
//   def castTest2(e1: ExprBase { type A = Int })(e2: e1.type)(x: e1.A): e2.A = x
//   def castTest3(e1: ExprBase)(e2: ExprBase with e1.type)(x: e2.A): e1.A = x

//   def castTest4(e1: ExprBase { type A = Int })(e2: ExprBase with e1.type)(x: e2.A): e1.A = x

//   def castTest5a(e1: ExprBase)(e2: LitU with e1.type)(x: e2.A): e1.A = x
//   def castTest5b(e1: ExprBase)(e2: LitL with e1.type)(x: e2.A): e1.A = x

//   //fail:
//   def castTestFail1(e1: ExprBase)(e2: Lit with e1.type)(x: e2.A): e1.A = x // this is like castTest5a/b, but with Lit instead of LitU/LitL
//   // the other direction never works:
//   def castTestFail2a(e1: ExprBase)(e2: Lit with e1.type)(x: e1.A): e2.A = x
//   def castTestFail2b(e1: ExprBase)(e2: LitL with e1.type)(x: e1.A): e2.A = x
//   def castTestFail2c(e1: ExprBase)(e2: LitU with e1.type)(x: e1.A): e2.A = x

//   // the problem isn't about order of intersections.
//   def castTestFail2bFlip(e1: ExprBase)(e2: e1.type with LitL)(x: e1.A): e2.A = x
//   def castTestFail2cFlip(e1: ExprBase)(e2: e1.type with LitU)(x: e1.A): e2.A = x

//   def castTestFail3(e1: ExprBase)(e2: Lit with e1.type)(x: e1.A): e2.A = {
//     val y: e1.type with e2.type = e2
//     val z = x: y.A
//     z
//   }
// }

object Source {
  trait Expr[A]
  case class Lit(n: Int) extends Expr[Int]
  case class Plus(lhs: Expr[Int], rhs: Expr[Int]) extends Expr[Int]
  case class Var[A](a: A) extends Expr[A]
  case class Fun[A, B](fun: Expr[A] => Expr[B]) extends Expr[A => B]
  case class App[A, B](fun: Expr[A => B], arg: Expr[A]) extends Expr[B]

  def eval[A](e: Expr[A]): A = e match {
    case Lit(n) => n
    case Plus(a,b) => eval(a) + eval(b)
    case Var(x) => x
    case f: Fun[a,b] =>
      (x: a) => eval(f.fun(Var(x)))
    case App(fun,arg) => eval(fun)(eval(arg))
  }
}

object Encoded {
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

  // def eval(e: ExprBase): e.A = e match {
  //   case l: (Lit & e.type) => l.n
  //   case v: (Var & e.type) => v.a
  //   case p: (Plus & e.type) =>
  //     //we can't sum them.
  //     eval(p.lhs) + eval(p.rhs)
  //   case a: (App & e.type) =>
  //     eval(a.fun)(eval(a.arg))
  //   case f: (Fun & e.type) =>
  //     (x: f.B) => {
  //       val w = f.fun(new Var {type A = f.B; val a = x })
  //       eval(w)
  //     }
  // }
}

object EncodedWorkingish {
  type Expr[+a] = ExprBase { type A <: a }
  abstract class ExprBase { s =>
    type A
  }

  abstract class Lit extends ExprBase { s =>
    type A <: Int //A = Int breaks
    val n: A
  }

  abstract class Plus extends ExprBase { s =>
    type A <: Int //A = Int breaks
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

  def eval(e: ExprBase): e.A = e match {
    case l: (Lit & e.type) => l.n
    case v: (Var & e.type) => v.a
    case p: (Plus & e.type) =>
      //we can't sum them.
      eval(p.lhs): p.A
      eval(p.rhs): p.A
    case a: (App & e.type) =>
      eval(a.fun)(eval(a.arg))
    // case f: (Fun & e.type) =>
    //   (((x: f.B) => {
    //     val w = f.fun(new Var {type A = f.B; val a = x})
    //     eval(w)
    //   }): f.A): e.A //: (f.B => f.C)
  }
}
