import scala.language.implicitConversions
object Figure2 {
  sealed abstract class Expr[A] {
    def `match`[R](
      Lit: (given Int <:< A) => Lit => R,
      Plus: (given Int <:< A) => Plus => R,
      App: [B] => App[B,A] => R,
      Fun: [B,C] => (given (B => C) <:< A) => Fun[B,C] => R,
      Var: Var[A] => R ): R
  }

  final class Lit(val n: Int) extends Expr[Int] {

    def `match`[R](
      Lit: (given Int <:< Int) => Lit => R,
      Plus: (given Int <:< Int) => Plus => R,
      App: [B] => App[B,Int] => R,
      Fun: [B,C] => (given (B => C) <:< Int) => Fun[B,C] => R,
      Var: Var[Int] => R ): R =
        Lit.apply(this)

  }

  final class Plus(val lhs: Expr[Int], val rhs: Expr[Int]) extends Expr[Int] {

    def `match`[R](
      Lit: (given Int <:< Int) => Lit => R,
      Plus: (given Int <:< Int) => Plus => R,
      App: [B] => App[B,Int] => R,
      Fun: [B,C] => (given (B => C) <:< Int) => Fun[B,C] => R,
      Var: Var[Int] => R ): R =
        Plus.apply(this)

  }

  final class Var[A](val a: A) extends Expr[A] {

    def `match`[R](
      Lit: (given Int <:< A) => Lit => R,
      Plus: (given Int <:< A) => Plus => R,
      App: [B] => App[B,A] => R,
      Fun: [B,C] => (given (B => C) <:< A) => Fun[B,C] => R,
      Var: Var[A] => R ): R =
        Var.apply(this)

  }

  final class Fun[A,B](val fun: Expr[A] => Expr[B]) extends Expr[A => B] {

    def `match`[R](
      Lit: (given Int <:< (A => B)) => Lit => R,
      Plus: (given Int <:< (A => B)) => Plus => R,
      App: [B_] => App[B_,(A => B)] => R,
      Fun: [B_,C_] => (given (B_ => C_) <:< (A => B)) => Fun[B_,C_] => R,
      Var: Var[(A => B)] => R ): R =
        Fun.apply(this)

  }

  final class App[A,B](val fun: Expr[A => B], val arg: Expr[A]) extends Expr[B] {

    def `match`[R](
      Lit: (given Int <:< B) => Lit => R,
      Plus: (given Int <:< B) => Plus => R,
      App: [B_] => App[B_,B] => R,
      Fun: [B_,C_] => (given (B_ => C_) <:< B) => Fun[B_,C_] => R,
      Var: Var[B] => R ): R =
        App.apply(this)

  }

  def eval[A](e: Expr[A]): A = e.`match`[A](
    Lit = (given sub) => l => sub(l.n),
    Plus = (given sub) => p => sub(eval(p.lhs) + eval(p.rhs)),
    App = [B] => (a: App[B, A]) => eval(a.fun).apply(eval(a.arg)),
    Fun = [B,C] => (given ev: (B => C) <:< A) => (f: Fun[B, C]) => ev( (x: B) => eval[C](f.fun(Var(x))) ),
    Var = v => v.a
  )
}
