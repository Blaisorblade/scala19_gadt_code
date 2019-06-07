enum Expr[A] {
  case Lit(n: Int) extends Expr[Int]
  case Plus(lhs: Expr[Int], rhs: Expr[Int]) extends Expr[Int]
  case Var(a: A) extends Expr[A]
  case Fun[A, B](fun: Expr[A] => Expr[B]) extends Expr[A => B]
  case App[A, B](fun: Expr[A => B], arg: Expr[A]) extends Expr[B]
}
object Expr {
  def eval[A](e: Expr[A]): A = e match {
    case Lit(n) => n
    case Plus(a,b) => eval(a) + eval(b)
    case Var(x) => x
    case f: Fun[a,b] =>
      (x: a) => eval(f.fun(Var(x)))
    case App(fun,arg) => eval(fun)(eval(arg))
  }
}
