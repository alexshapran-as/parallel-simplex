package parallel_simplex.util

import scala.util.Try

// -----------------------------------------------------------------------------------------------------------------
// Методы работы с числами
// -----------------------------------------------------------------------------------------------------------------

object NumbersUtils {
    val zeroEpsilon = 0.000001

    def roundWithScale(decimal: Double, scale: Int): Double = Try {
        BigDecimal(decimal).setScale(scale, BigDecimal.RoundingMode.HALF_UP).toDouble
    } recover {
        case e: Exception => sys.error(e.getMessage)
    } get

    def isNumberZero(number: Double): Boolean = math.abs(number) <= zeroEpsilon

    def number2number(number: Double): Double = if (isNumberZero(number)) 0.0 else number
}
