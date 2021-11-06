package parallel_simplex.util

import GPU._
import parallel_simplex.models.Matrix

import scala.annotation.tailrec
import scala.util.Try

// -------------------------------------------------------------------------------------------------------------
// * Симплекс
// -------------------------------------------------------------------------------------------------------------

object SimplexUtils {
    var gpuUtils: SimplexGPUUtils = null

    def init(
                matrix: Matrix,
                basicVariables: Array[Int],
                variables: Array[Int]
            ): Unit = {
        gpuUtils = new SimplexGPUUtils(
            matrix.matrix,
            matrix.getMatrixRowsCount,
            matrix.getMatrixColumnsCount,
            basicVariables,
            variables
        )
    }

    def solve(): Either[String, (Array[Double], Array[Int], Array[Int], Double)] = {
        findReferenceSolution() flatMap { _ =>
            findOptimalSolution() map { _ =>
                (
                    gpuUtils.getSolution,
                    gpuUtils.getBasicVariables,
                    gpuUtils.getVariables,
                    gpuUtils.getObjectiveFunction
                )
            }
        }
    }

    // Поиск опорного решения
    @tailrec
    private def findReferenceSolution(): Either[String, Unit] = {
        // поиск разрешающей строки и разрешающего столбца
        // выделение столбца свободных членов (s_i_0) выполняется на GPU
        // поиск отрицательного элемента в этом столбце (кроме элемента в строке F)
        // выполняется на CPU [Efficient Implementation of the Simplex Method on a CPU-GPU System III.B]
        val rowsCount = gpuUtils.getRowsCount
        val columnsCount = gpuUtils.getColumnsCount
        findIndex(
            gpuUtils.getFreeColumn,
            index = 0,
            length = rowsCount,
            stopCondition = (elem, index) => elem < 0.0 && index != rowsCount - 1
        ) match {
            // для хвостовой рекурсии
            case None => Right()
            case Some(freeRowIndex) =>
                // взять из GPU строку под индексом freeRowIndex
                // поиск отрицательного элемента в соответствующей строке без первого элемента (s_freeRowIndex_0)
                findIndex(
                    gpuUtils.getRow(freeRowIndex),
                    index = 0,
                    length = columnsCount,
                    stopCondition = (elem, index) => elem < 0.0 && index != 0
                ) match {
                    // для хвостовой рекурсии
                    case None => Left("Допустимых решений не существует")
                    case Some(permissiveColumnIndex) =>
                        // getPermissiveRowIndex выполняется на GPU, ответ - массив результатов делений
                        // а поиск минимума выполняется на CPU
                        // поиск минимального положительного отношения
                        // эл-та свободных членов (s_i_0) к соответствующему эл-ту в разрешающем столбце (без строки F)
                        getPermissiveRowIndex(permissiveColumnIndex) match {
                            // для хвостовой рекурсии
                            case None => Left("Разрешающая строка не найдена")
                            // permissiveRowIndex - индекс разрешающей строки
                            case Some(permissiveRowIndex) =>
                                // преобразования элементов выполняются на GPU
                                // передать на GPU permissiveRowIndex и permissiveColumnIndex
                                // поменять местами свободную и базисную переменные
                                // меняются непосредственно массивы basicVariables и variables
                                // (копировать нет необходимости)
                                gpuUtils.baseChange(permissiveRowIndex, permissiveColumnIndex - 1)
                                // Жордановы исключения
                                gpuUtils.computeJordanExceptions(permissiveRowIndex, permissiveColumnIndex)
                                findReferenceSolution()
                        }
                }
        }
    }

    // Поиск оптимального решения
    @tailrec
    private def findOptimalSolution(): Either[String, Unit] = {
        // Аналогично предыдущему шагу выполняются поиск
        // permissiveRowIndex и permissiveColumnIndex на CPU
        // а смена базисов и Жордановы исключения - на GPU

        val rowsCount = gpuUtils.getRowsCount
        val columnsCount = gpuUtils.getColumnsCount

        // поиск разрешающего столбца по последней строке без свободного члена
        // (поиск первого положительного элемента в строке)
        findIndex(
            gpuUtils.getRow(rowsCount - 1),
            index = 0,
            length = columnsCount,
            stopCondition = (elem, index) => elem > 0.0 && index != 0
        ) match {
            case None => Right()
            // permissiveColumnIndex - индекс разрешающего столбца
            case Some(permissiveColumnIndex) =>
                // поиск минимального положительного отношения
                // эл-та свободных членов (s_i_0) к соответствующему эл-ту в разрешающем столбце
                // (без строки F и без учета отношений, где s_i_k < 0)
                getPermissiveRowIndex(permissiveColumnIndex, skipByNegativeDivision = true) match {
                    case None => Left("Разрешающая строка не найдена")
                    // permissiveRowIndex - индекс разрешающей строки
                    case Some(permissiveRowIndex) =>
                        // меняем местами свободную и базисную переменные
                        gpuUtils.baseChange(permissiveRowIndex, permissiveColumnIndex - 1)
                        // Жордановы исключения
                        gpuUtils.computeJordanExceptions(permissiveRowIndex, permissiveColumnIndex)
                        findOptimalSolution()
                }
        }
    }

    // Поиск разрешающей строки по разрешающему столбцу
    private def getPermissiveRowIndex(
                                         permissiveColumnIndex: Int,
                                         skipByNegativeDivision: Boolean = false
                                     ): Option[Int] = Try {
        val free2PermissiveRatios = gpuUtils.getColumnsRatios(
            0, permissiveColumnIndex,
            Double.NaN,
            if (skipByNegativeDivision) 1 else 0, Double.NaN
        )
        // поиск индекса строки, в которой отношение
        // эл-та свободных членов (s_i_0) к соответствующему эл-ту в разрешающем столбце
        // минимально и положительно (кроме строки F)
        val rowsCount = gpuUtils.getRowsCount
        var minRatioValue: Option[Double] = None
        var minRatioIndex: Option[Int] = None
        var index = 0
        free2PermissiveRatios.foreach { ratio =>
                if (ratio != Double.NaN && ratio >= 0.0 && index != rowsCount - 1) {
                    if (minRatioValue.isEmpty) {
                        minRatioValue = Some(ratio)
                        minRatioIndex = Some(index)
                    } else if (minRatioValue.get >= ratio) {
                        minRatioValue = Some(ratio)
                        minRatioIndex = Some(index)
                    }
                }
            index += 1
        }
        minRatioIndex
    }.recover {
        case exception: Exception =>
            parallel_simplex.logger.error(exception.getMessage)
            None
    }.get

    @tailrec
    final def findIndex(
                           arr: Array[Double],
                           index: Int,
                           length: Int,
                           stopCondition: (Double, Int) => Boolean
                       ): Option[Int] = {
        if (index >= length) None
        else if (stopCondition(arr(index), index)) Some(index)
        else findIndex(arr, index + 1, length, stopCondition)
    }
}
