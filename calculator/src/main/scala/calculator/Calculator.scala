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
    namedExpressions.map {
      case (varName,exprSignal) => varName -> Signal(eval(exprSignal(),namedExpressions))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]],seen :Set[Expr] = Set()): Double = {
    expr match {
      case Literal(v)  => v
      case Ref(name)   =>
        if(seen.contains(Ref(name))) Double.NaN
        else eval(getReferenceExpr(name,references),references, seen + Ref(name))
      case Plus(a,b)   => eval(a,references,seen) + eval(b,references,seen)
      case Minus(a,b)  => eval(a,references,seen) - eval(b,references,seen)
      case Times(a,b)  => eval(a,references,seen) * eval(b,references,seen)
      case Divide(a,b) => eval(a,references,seen) / eval(b,references,seen)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
