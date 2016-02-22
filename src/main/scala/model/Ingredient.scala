package me.krobinson.mealplan.model

import org.apache.commons.lang.math.Fraction

import scala.util.Try
import scalaz.{-\/, \/-}
import scalaz.syntax.id._

sealed trait Measurement {
  def +(that: Measurement): Measurement
  lazy val classType = this.getClass.getSimpleName.toLowerCase
}

trait Amount extends Measurement {
  val amount: Double
  val companion: MeasurementConstructor
  def +(that: Measurement): Measurement =
    that match {
      case a: Amount => companion.construct(this.amount + a.amount)
      case _         => this
    }

  override def toString = s"$amount $classType"
}

trait MeasurementConstructor {
  val keys: Seq[String]
  def construct(amount: Double): Measurement
  def keywords: Seq[String] = keys ++ keys.map(_ + "s")
}

case class Tsp(amount: Double) extends Measurement with Amount {
  val companion: MeasurementConstructor = Tsp
}
object Tsp extends MeasurementConstructor {
  val keys = Seq("tsp")
  def construct(amount: Double): Tsp = Tsp(amount)
}

case class Tbsp(amount: Double) extends Measurement with Amount {
  val companion: MeasurementConstructor = Tbsp
}
object Tbsp extends MeasurementConstructor {
  val keys = Seq("tbsp")
  def construct(amount: Double): Tbsp = Tbsp(amount)
}

case class Cup(amount: Double) extends Measurement with Amount {
  def asTbsp: Tbsp = Tbsp(amount * 16)
  val companion: MeasurementConstructor = Cup
}
object Cup extends MeasurementConstructor {
  val keys = Seq("cup")
  def construct(amount: Double): Cup = Cup(amount)
}

case class Ounce(amount: Double) extends Measurement with Amount {
  val companion: MeasurementConstructor = Ounce
  def asTsp = Tsp(amount * 6)
}
object Ounce extends MeasurementConstructor {
  val keys = Seq("oz", "ounce")
  def construct(amount: Double): Ounce = Ounce(amount)
}

case class Lb(amount: Double) extends Measurement with Amount {
  def asTbsp: Tbsp = Tbsp(amount * 32)
  val companion: MeasurementConstructor = Lb
  def asTsp = this.asTbsp.companion
}
object Lb extends MeasurementConstructor {
  val keys = Seq("lb")
  def construct(amount: Double): Lb = Lb(amount)
}

case class Clove(amount: Double) extends Measurement with Amount {
  val companion: MeasurementConstructor = Clove
}
object Clove extends MeasurementConstructor {
  val keys = Seq("clove")
  def construct(amount: Double): Clove = Clove(amount)
}

case class Pkg(size: Ounce) extends Measurement {
  def +(that: Measurement): Measurement =
    that match {
      case p: Pkg => Pkg(Ounce(this.size.amount + p.size.amount))
      case _      => this
    }
  override def toString = s"${size.amount} oz. package"
}
object Pkg extends MeasurementConstructor {
  val keys = Seq("pkg", "package")
  def construct(amount: Double): Pkg = Pkg(Ounce(amount))
}

case class Count(amount: Double) extends Measurement {
  def +(that: Measurement): Measurement =
    that match {
      case c: Count => Count(this.amount + c.amount)
      case _        => this
    }
  override def toString = s"$amount"
}

case class Can(amount: Double, size: Ounce) extends Measurement {
  def +(that: Measurement): Measurement =
    that match {
      case c: Can =>
        if (this.size == c.size) {
          Can(this.amount + c.amount, this.size)
        } else {
          val s = Math.min(this.size.amount, c.size.amount)
          val totalOz = (this.size.amount * this.amount) + (c.size.amount * c.amount)
          Can(Math.ceil(totalOz / s), Ounce(s))
        }
      case _      => this
    }

  override def toString = s"$amount $size oz. can"
}

case class OtherMeasurement(notes: List[String]) extends Measurement {
  def +(that: Measurement): Measurement =
    that match {
      case o: OtherMeasurement => OtherMeasurement(this.notes ++ o.notes)
      case _                   => this
    }

  override def toString = notes.mkString(",")
}

object Measurement {

  implicit class MeasurementExtension(m: Result[Measurement]) {
    def orDefault(original: String): Measurement =
      m match {
        case \/-(msr) => msr
        case -\/(err) => OtherMeasurement(List(original))// todo add logging ?
      }
  }

  val measurements: List[MeasurementConstructor] = List(Tsp, Tbsp, Cup, Ounce, Lb, Clove, Pkg)
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
    val fallback = OtherMeasurement(List(rawAmount))
    rawAmount.split(" ").toList match {
      case Nil    => fallback
      case x::Nil => Try(x.toDouble).toOption match {
        case Some(i) => Count(i)
        case None    => fallback
      }
      case xs =>
        val amount = xs.take(xs.length - 1).mkString(" ")
        val unit = xs.last.toLowerCase
        constructors.get(unit) match {
          case Some(constructor) => parseAmount(amount).map(constructor.construct).orDefault(rawAmount)
          case None if unit == "can" => parseCanAmount(amount).orDefault(rawAmount)
          case None => fallback
        }
    }
  }
}

case class Ingredient(
  name: String,
  category: String,
  measurement: Measurement
)

object Ingredient {

  def combineByName(l: List[Ingredient]): Map[(String, String), List[Ingredient]] =
    l.map { i =>
      val normalizedName = i.name.takeWhile(_ != ',').toLowerCase
      i.copy(name = normalizedName)
    } groupBy { g => (g.name, g.category) }

  def combineByMeasurement(l: List[Ingredient]): Map[String, List[Measurement]] =
    l.groupBy(_.measurement.classType).map { case (k,v) => (k, v.map(_.measurement)) }

  def reduceIngredients(l: List[Ingredient]): List[Ingredient] = {
    val grouped = combineByName(l).mapValues(combineByMeasurement)
    val reduced = grouped flatMap { case ((n,c),v) =>
      val measurements = v.map { case (a,m) => m.reduce(_ + _) }
      measurements map { m => Ingredient(n,c,m) }
    }
    reduced.toList
  }

}
