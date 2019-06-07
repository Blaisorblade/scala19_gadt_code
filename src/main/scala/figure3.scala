object Figure3 {
  type Expr[+a] = ExprBase { type A <: a }
  sealed abstract class ExprBase { s =>
    type A
    def `match`[R](
      Lit: (Lit & s.type) => R,
      Plus: (Plus & s.type) => R,
      App: (App & s.type) => R,
      Fun: (Fun & s.type) => R,
      Var: (Var & s.type) => R
    ): R
  }

  abstract class Lit extends ExprBase { s =>
    type A = Int
    val n: Int

    def `match`[R](
      Lit: (Lit & s.type) => R,
      Plus: (Plus & s.type) => R,
      App: (App & s.type) => R,
      Fun: (Fun & s.type) => R,
      Var: (Var & s.type) => R
    ): R =
        Lit.apply(this)

  }

  abstract class Plus extends ExprBase { s =>
    type A = Int
    val lhs: Expr[Int]
    val rhs: Expr[Int]

    def `match`[R](
      Lit: (Lit & s.type) => R,
      Plus: (Plus & s.type) => R,
      App: (App & s.type) => R,
      Fun: (Fun & s.type) => R,
      Var: (Var & s.type) => R
    ): R =
        Plus.apply(this)

  }

  abstract class Var extends ExprBase { s =>
    val a: A

    def `match`[R](
      Lit: (Lit & s.type) => R,
      Plus: (Plus & s.type) => R,
      App: (App & s.type) => R,
      Fun: (Fun & s.type) => R,
      Var: (Var & s.type) => R
    ): R =
        Var.apply(this)

  }

  abstract class Fun extends ExprBase { s =>
    type B
    type C
    type A = B => C
    val fun: Expr[B] => Expr[C]

    def `match`[R](
      Lit: (Lit & s.type) => R,
      Plus: (Plus & s.type) => R,
      App: (App & s.type) => R,
      Fun: (Fun & s.type) => R,
      Var: (Var & s.type) => R
    ): R =
        Fun.apply(this)

  }

  abstract class App extends ExprBase { s =>
    type B
    type C
    type A = C
    val fun: Expr[B => C]
    val arg: Expr[B]

    def `match`[R](
      Lit: (Lit & s.type) => R,
      Plus: (Plus & s.type) => R,
      App: (App & s.type) => R,
      Fun: (Fun & s.type) => R,
      Var: (Var & s.type) => R
    ): R =
        App.apply(this)

  }

def eval[A](e: Expr[A]): A = e.`match`[A](
  Lit = l => l.n: l.A,
  Plus = p => (eval(p.lhs) + eval(p.rhs)): p.A,
  App = a => eval(a.fun).apply(eval(a.arg)): a.A,
  Fun = f => ((x: f.B) => eval[f.C](f.fun(new Var { type A = f.B; val a = x }))): f.A,
  Var = v => v.a: v.A)
}
