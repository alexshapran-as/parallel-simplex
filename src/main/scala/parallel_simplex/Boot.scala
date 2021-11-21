package parallel_simplex

import org.slf4j.{Logger, LoggerFactory}
import parallel_simplex.models.Matrix
import parallel_simplex.util.{SimplexUtils, TablesUtils}

object Boot {
    protected val logger: Logger = LoggerFactory.getLogger(getClass)

    def main(args: Array[String]): Unit = {
        logger.info(s"***** Parallel simplex *****")

        // F = x_0 + x_1 + ... + x_m -> min
        // x_0 + ... + x_m >= 1;
        // x_0, ..., x_m - свободные переменные
        // variables - индексы свободных переменных (0, 1, ..., m)
        // basicVariables - индексы базисных переменных (m + 1, ..., n), n - число уравнений
        {
            val system = Matrix(Array(Array(1, 2, 7), Array(3, 6, 2), Array(9, 2, 6), Array(6,3,5)))
            val m = (system.matrix.map { row =>
                Array(-1.0) ++ row.map(x => -x)
            }) :+ Array.fill(system.getMatrixColumnsCount + 1)(-1.0).patch(0, Array(0.0), 1) // строка F
            val simplexMatrix = Matrix(m)
            val variables = (0 until system.getMatrixColumnsCount).toArray
            val basicVariables = (variables.length until variables.length + system.getMatrixRowsCount).toArray
            SimplexUtils.init(simplexMatrix, basicVariables, variables)
        }

        SimplexUtils.solve().map {
            case (solution, basicVariablesSolution, variablesSolution, objFunction) =>
                val solutionMsg = {
                    val varValues = solution zip (basicVariablesSolution ++ variablesSolution) map {
                        case (value, variable) => s"x_$variable = $value;"
                    }
                    val objFValue = s"F = $objFunction."
                    varValues :+ objFValue mkString("\n")
                }
                println("Решение:\n" + solutionMsg)
        }
    }
}
