import scala.language.implicitConversions
object Figure2p {
  sealed abstract class Expr[A] {
    def `match`[R]: (
      Lit: (given Int <:< A) => Lit => R,
      Plus: (given Int <:< A) => Plus => R,
      App: [B] => App[B,A] => R,
      Fun: [B,C] => (given (B => C) <:< A) => Fun[B,C] => R,
      Var: Var[A] => R) => R
  }

  final class Lit(val n: Int) extends Expr[Int] {
    def `match`[R] =
      (Lit, Plus, App, Fun, Var) => Lit.apply(this)
  }

  final class Plus(val lhs: Expr[Int], val rhs: Expr[Int]) extends Expr[Int] {

    def `match`[R] =
      (Lit, Plus, App, Fun, Var) => Plus.apply(this)
  }

  final class Var[A](val a: A) extends Expr[A] {
    def `match`[R] =
      (Lit, Plus, App, Fun, Var) => Var.apply(this)
  }

  final class Fun[A,B](val fun: Expr[A] => Expr[B]) extends Expr[A => B] {
    def `match`[R] =
      (Lit, Plus, App, Fun, Var) => Fun.apply(this)
  }

  final class App[A,B](val fun: Expr[A => B], val arg: Expr[A]) extends Expr[B] {

    def `match`[R] =
      (Lit, Plus, App, Fun, Var) => App.apply(this)
  }

  def eval[A](e: Expr[A]): A = e.`match`[A](
    (given sub) => l => sub(l.n),
    (given sub) => p => sub(eval(p.lhs) + eval(p.rhs)),
    [B] => (a: App[B, A]) => eval(a.fun).apply(eval(a.arg)),
    [B,C] => (given ev: (B => C) <:< A) => (f: Fun[B, C]) => ev( (x: B) => eval[C](f.fun(Var(x))) ),
    v => v.a
  )
}
