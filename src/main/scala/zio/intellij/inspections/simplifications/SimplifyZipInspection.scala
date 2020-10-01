package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScInfixExpr}
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods._
import zio.intellij.utils.StringUtils._

class SimplifyZipInspection
    extends ZInspection(
      ZipRightSimplificationType,
      ZipLeftSimplificationType,
      ZipRightOperatorSimplificationType,
      ZipLeftOperatorSimplificationType
    )

sealed class BaseZipOneSimplificationType(invocation: Qualified, replaceWith: String) extends SimplificationType {

  override def hint: String = s"Replace with .$replaceWith"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case qual invocation `_ => x`(x) =>
        Some(replace(expr).withText(invocationText(qual, s"$replaceWith${x.getWrappedText}")))
      case _ => None
    }

}

sealed class BaseZipOneOperatorSimplificationType(invocation: Qualified, replaceWith: String)
    extends SimplificationType {

  override def hint: String = s"Replace with $replaceWith"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {

    def replacement(qual: ScExpression, x: ScExpression) =
      x match {
        case _: ScInfixExpr => replace(expr).withText(s"${qual.getBracedText} $replaceWith (${x.getBracedText})")
        case _              => replace(expr).withText(s"${qual.getBracedText} $replaceWith ${x.getBracedText}")
      }

    expr match {
      case qual invocation `_ => x`(x) => Some(replacement(qual, x))
      case _                           => None
    }
  }

}

object ZipRightSimplificationType extends BaseZipOneSimplificationType(`.flatMap`, "zipRight")

object ZipLeftSimplificationType extends BaseZipOneSimplificationType(`.tap`, "zipLeft")

object ZipRightOperatorSimplificationType extends BaseZipOneOperatorSimplificationType(`.flatMap`, "*>")

object ZipLeftOperatorSimplificationType extends BaseZipOneOperatorSimplificationType(`.tap`, "<*")
