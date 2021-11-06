package parallel_simplex.util

// -----------------------------------------------------------------------------------------------------------------
// Методы работы с таблицами
// -----------------------------------------------------------------------------------------------------------------

object TablesUtils {
    def formatTable[T](table: Array[Array[T]]): String = table match {
        case Array() =>
            ""
        case _ =>
            val sizes = table.map(row => row.map(element =>  if (element == null) 0 else element.toString.length))
            val columnsSizes = sizes.transpose.map(size => size.max)
            val rows = table.map(row => formatRow(row, columnsSizes))
            formatRows(rowSeparator(columnsSizes), rows)
    }

    private def formatRows(rowSeparator: String, rows: Array[String]): String = {
        (rows.map(row => List(rowSeparator, row).mkString("\n")) :+ rowSeparator).mkString("\n")
    }

    private def formatRow[T](row: Array[T], columnsSizes: Array[Int]): String = {
        val columns = row.zip(columnsSizes).map { case (element, size) =>
            if (size == 0) "" else ("%" + size + "s").format(element)
        }
        columns.mkString("|", "|", "|")
    }

    private def rowSeparator(columnsSizes: Array[Int]): String = columnsSizes.map("-" * _).mkString("+", "+", "+")
}
