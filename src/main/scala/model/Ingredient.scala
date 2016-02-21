package me.krobinson.mealplan.model

import org.apache.commons.lang.math.Fraction

import scala.util.Try
import scalaz.{-\/, \/-}
import scalaz.syntax.id._

sealed trait Measurement

trait MeasurementConstructor {
  val keys: Seq[String]
  def construct(amount: Double): Measurement
  def keywords: Seq[String] = keys ++ keys.map(_ + "s")
}

case class Tsp(amount: Double) extends Measurement
object Tsp extends MeasurementConstructor {
  val keys = Seq("tsp")
  def construct(amount: Double): Tsp = Tsp(amount)
}

case class Tbsp(amount: Double) extends Measurement
object Tbsp extends MeasurementConstructor {
  val keys = Seq("tbsp")
  def construct(amount: Double): Tbsp = Tbsp(amount)
}

case class Cup(amount: Double) extends Measurement
object Cup extends MeasurementConstructor {
  val keys = Seq("cup")
  def construct(amount: Double): Cup = Cup(amount)
}

case class Ounce(amount: Double) extends Measurement
object Ounce extends MeasurementConstructor {
  val keys = Seq("oz", "ounce")
  def construct(amount: Double): Ounce = Ounce(amount)
}

case class Liter(amount: Double) extends Measurement
object Liter extends MeasurementConstructor {
  val keys = Seq("liter", "litre")
  def construct(amount: Double): Liter = Liter(amount)
}

case class Lb(amount: Double) extends Measurement
object Lb extends MeasurementConstructor {
  val keys = Seq("lb")
  def construct(amount: Double): Lb = Lb(amount)
}

case class Clove(amount: Double) extends Measurement
object Clove extends MeasurementConstructor {
  val keys = Seq("clove")
  def construct(amount: Double): Clove = Clove(amount)
}

case class Pkg(size: Ounce) extends Measurement

object Pkg extends MeasurementConstructor {
  val keys = Seq("package", "pkg")
  def construct(amount: Double): Pkg = Pkg(Ounce(amount))
}

case class Count(amount: Double) extends Measurement

case class Can(num: Double, size: Ounce) extends Measurement

case class OtherMeasurement(note: String) extends Measurement

object Measurement {

  implicit class MeasurementExtension(m: Result[Measurement]) {
    def orDefault(original: String): Measurement =
      m match {
        case \/-(msr) => msr
        case -\/(err) => OtherMeasurement(original)// todo add logging ?
      }
  }

  val measurements: List[MeasurementConstructor] = List(Tsp, Tbsp, Cup, Ounce, Liter, Lb, Clove, Pkg)
  lazy val constructors: Map[String, MeasurementConstructor] = measurements.foldLeft(Map.empty[String, MeasurementConstructor]) {
    case (acc,cnst) => acc ++ cnst.keywords.map { k => (k, cnst) }
  }

  def parseCanAmount(amount: String): Result[Can] = {
    val canAmt = """([0-9]+) \(?([0-9]+) oz\.?\)?""".r
    amount match {
      case canAmt(amt, ounces) =>
        Try((amt.toDouble, ounces.toDouble)).toOption match {
          case Some((a,o)) => Can(a,Ounce(o)).right
          case None        => s"Expecting numeric values for can sizes: $amount".left
        }
      case _ => "Unrecognized format".left
    }
  }

  def parseAmount(amount: String): Result[Double] = {
    Try(Fraction.getFraction(amount)).toOption match {
      case Some(f) => f.doubleValue().right
      case None    => s"Unsupported parse value at this time: $amount".left
    }
  }

  def apply(rawAmount: String): Measurement = {
    rawAmount.split(" ").toList match {
      case Nil    => OtherMeasurement(rawAmount)
      case x::Nil => Try(x.toDouble).toOption match {
        case Some(i) => Count(i)
        case None    => OtherMeasurement(rawAmount)
      }
      case xs =>
        val amount = xs.take(xs.length - 1).mkString(" ")
        val unit = xs.last.toLowerCase
        constructors.get(unit) match {
          case Some(constructor) => parseAmount(amount).map(constructor.construct).orDefault(rawAmount)
          case None if unit == "can" => parseCanAmount(amount).orDefault(rawAmount)
          case None => OtherMeasurement(rawAmount)
        }
    }
  }
}

case class Ingredient(
  name: String,
  category: String,
  measurement: Measurement
)