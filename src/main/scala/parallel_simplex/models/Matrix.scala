package parallel_simplex.models

case class Matrix(matrix: Array[Array[Double]]) {
    def getMatrixRowsCount: Int = matrix.length

    def getMatrixColumnsCount: Int = {
        val matrixColumnsCounts = matrix.map(matrixRow => matrixRow.length)
        matrixColumnsCounts.distinct match {
            case Array(matrixColumnsCount) => matrixColumnsCount
            case Array() => 0
            case Array(_*) => throw new Exception("Матрица задана неверно. Количество столбцов в разных строках отличается")
        }
    }

    def transpose: Matrix = this.copy(matrix.transpose)
}

object Matrix {
//    def fromPrimitive(
//                         matrixPrimitive: Array[Double],
//                         rowsCount: Int,
//                         columnsCount: Int
//                     ): Matrix = {
//        Matrix(
//            (0 until rowsCount).toArray.map { rowIndex =>
//                (0 until columnsCount).toArray.map { columnIndex =>
//                    matrixPrimitive(rowIndex * columnsCount + columnIndex)
//                }
//            }
//        )
//    }

    // TODO: запись из файла
    def fromFile() = {}
}

