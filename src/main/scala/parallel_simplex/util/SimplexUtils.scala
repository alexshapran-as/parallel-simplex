package parallel_simplex.util

import GPU._
import parallel_simplex.models.Matrix

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.{Success, Try}

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

    def solveCPU(
                    simplexMatrix: Matrix,
                    basicVariables: Array[Int],
                    variables: Array[Int]
                ): Either[String, (Array[Double], Array[Int], Array[Int], Double)] = {

        // -------------------------------------------------------------------------------------------------------------
        // * Симплекс
        // -------------------------------------------------------------------------------------------------------------

        val zeroEpsilon = 0.000001

        def roundWithScale(decimal: Double, scale: Int): Double =
            BigDecimal(decimal).setScale(scale, BigDecimal.RoundingMode.HALF_UP).toDouble

        def isNumberZero(number: Double): Boolean = math.abs(number) <= zeroEpsilon

        // Поиск разрешающей строки по разрешающему столбцу
        def getPermissiveRowIndex(matrix: Matrix, permissiveColumnIndex: Int, skipByNegDiv: Boolean = false): Option[Int] = Try {
            val matrixTransposed = matrix.transpose
            val freeColumnValues = matrixTransposed.matrix.head.init
            val permissiveColumnValues = matrixTransposed.matrix(permissiveColumnIndex).init
            val permissiveRowIndex = freeColumnValues.zip(permissiveColumnValues)
                .zipWithIndex
                .filter(x => !isNumberZero(x._1._2))
                .filter(x => if (skipByNegDiv) x._1._2 >= 0 else true)
                .map {
                    case ((freeColumnValue, permissiveColumnValue), rowIndex) =>
                        (freeColumnValue / permissiveColumnValue, rowIndex)
                }.filter(_._1 >= 0.0).minBy(_._1)._2
            Some(permissiveRowIndex)
        }.recover {
            case exception: Exception =>
                println(exception.getMessage)
                None
        }.get

        // Замена базиса
        def changeBasicVariables[T: ClassTag](basicVariables: Array[T],
                                                      variables: Array[T],
                                                      permissiveRowIndex: Int,
                                                      permissiveColumnIndex: Int): (Array[T], Array[T]) = {
            (
                basicVariables.patch(permissiveRowIndex, Array[T](variables(permissiveColumnIndex - 1)), 1),
                variables.patch(permissiveColumnIndex - 1, Array[T](basicVariables(permissiveRowIndex)), 1)
            )
        }

        // Жордановы исключения
        def computeJordanExceptions(matrix: Matrix,
                                            permissiveRowIndex: Int,
                                            permissiveColumnIndex: Int): Matrix = {
            val matrixCopy = matrix
            Matrix(matrixCopy.matrix.zipWithIndex.map { case (row, rowIndex) =>
                row.zipWithIndex.map { case (value, columnIndex) =>
                    if (rowIndex == permissiveRowIndex && columnIndex == permissiveColumnIndex) {
                        roundWithScale(1 / matrix.matrix(permissiveRowIndex)(permissiveColumnIndex), 5)
                    } else if (rowIndex == permissiveRowIndex) {
                        roundWithScale(value / matrix.matrix(permissiveRowIndex)(permissiveColumnIndex), 5)
                    } else if (columnIndex == permissiveColumnIndex) {
                        roundWithScale(-value / matrix.matrix(permissiveRowIndex)(permissiveColumnIndex), 5)
                    } else {
                        roundWithScale(value - ((row(permissiveColumnIndex) * matrix.matrix(permissiveRowIndex)(columnIndex)) / matrix.matrix(permissiveRowIndex)(permissiveColumnIndex)), 5)
                    }
                }
            })
        }

        // Поиск опорного решения
        @tailrec
        def findReferenceSolutionCPU[T: ClassTag](matrix: Matrix,
                                                  basicVariables: Array[T],
                                                  variables: Array[T]): Option[(Matrix, Array[T], Array[T])] = {
            val matrixTransposed = matrix.transpose
            matrixTransposed.matrix.head.zipWithIndex.init.find(_._1 < 0.0) match {
                case Some((_, freeRowIndex)) =>
                    matrix.matrix(freeRowIndex).zipWithIndex.tail.find(_._1 < 0.0) match {
                        case Some((_, permissiveColumnIndex)) =>
                            getPermissiveRowIndex(matrix, permissiveColumnIndex) match {
                                case Some(permissiveRowIndex) =>
                                    val (newBasicVariables, newVariables) = changeBasicVariables(basicVariables, variables, permissiveRowIndex, permissiveColumnIndex)
                                    val newMatrix = computeJordanExceptions(matrix, permissiveRowIndex, permissiveColumnIndex)
                                    findReferenceSolutionCPU(newMatrix, newBasicVariables, newVariables)
                                case _ =>
                                    None
                            }
                        case _ =>
                            None
                    }
                case _ =>
                    Some(matrix, basicVariables, variables)
            }
        }

        // Поиск оптимального решения
        @tailrec
        def findOptimalSolutionCPU[T: ClassTag](matrix: Matrix,
                                                basicVariables: Array[T],
                                                variables: Array[T]): Option[(Matrix, Array[T], Array[T])] = {
            matrix.matrix.last.zipWithIndex.tail.find(_._1 > 0) match {
                case Some((_, permissiveColumnIndex)) =>
                    getPermissiveRowIndex(matrix, permissiveColumnIndex, true) match {
                        case Some(permissiveRowIndex) =>
                            val (newBasicVariables, newVariables) = changeBasicVariables(basicVariables, variables, permissiveRowIndex, permissiveColumnIndex)
                            val newMatrix = computeJordanExceptions(matrix, permissiveRowIndex, permissiveColumnIndex)
                            findOptimalSolutionCPU(newMatrix, newBasicVariables, newVariables)
                        case _ =>
                            None
                    }
                case _ =>
                    Some(matrix, basicVariables, variables)
            }
        }

        def simplex[T: ClassTag](matrix: Matrix,
                                 basicVariables: Array[T],
                                 variables: Array[T]): Option[(Matrix, Array[T], Array[T])] = {
            findReferenceSolutionCPU(matrix, basicVariables, variables) flatMap {
                case (matrixReference, basicVariablesReference, variablesReference) =>
                    findOptimalSolutionCPU(matrixReference, basicVariablesReference, variablesReference)
            }
        }

        Try { simplex(simplexMatrix, basicVariables, variables) } match {
            case Success(Some((matrixSolution, basicVariablesSolution, variablesSolution))) =>
                val solution = matrixSolution.transpose.matrix.head.init ++ Array.fill(matrixSolution.getMatrixColumnsCount - 1)(0.0)
                val objectiveFunction = matrixSolution.matrix.last.head
                Right((solution, basicVariablesSolution, variablesSolution, objectiveFunction))
            case _ =>
                Left("Some error")
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
            // на GPU нет возможности работать с Boolean, поэтому преобразуем в 0 или 1
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
