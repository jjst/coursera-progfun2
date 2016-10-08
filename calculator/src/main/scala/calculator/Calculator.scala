package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map { case (name, expression) =>
      (name, Signal(eval(expression(), namedExpressions)))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def e(expr: Expr): Double = eval(expr, references)
    expr match {
      case Literal(value) => value
      case Ref(name) => eval(getReferenceExpr(name, references), references - name)
      case Plus(a, b) => e(a) + e(b)
      case Minus(a, b) => e(a) + e(b)
      case Times(a, b) => e(a) * e(b)
      case Divide(a, b) => e(a) / e(b)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
