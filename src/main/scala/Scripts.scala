import GPU.SimplexGPUUtils
import parallel_simplex.models.Matrix
import parallel_simplex.util.NumbersUtils

object GPUTest {
    def main(args: Array[String]): Unit = {
//        val m = Matrix(Array(Array(1, 2, 3, 4), Array(5, 6, 7, 8), Array(9, 10, 11, 12)))
//        val u = new SimplexGPUUtils(m.matrix, m.getMatrixRowsCount, m.getMatrixColumnsCount, Array.empty[Int], Array.empty[Int])
//        val row1 = u.getRow(1)
//        println(row1.mkString(","))
//        val ratios = u.getColumnsRatios(0, 1, NumbersUtils.zeroEpsilon * (-1.0))
//        println(ratios.mkString(","))
//        u.computeJordanExceptions(0,0)
    }
}

object RK1 {
    def main(args: Array[String]): Unit = {
        def getVM(n: BigInt, a: BigInt, m: Int, x: BigInt, y: BigInt): BigInt = {
            m.toBinaryString.toList.tail.foldLeft((x, y)) { case ((xAcc, yAcc), bit) =>
                if (bit == '1') (((xAcc * yAcc) - a).mod(n), ((yAcc * yAcc) - 2).mod(n))
                else (((xAcc * xAcc) - 2).mod(n), ((xAcc * yAcc) - a).mod(n))
            }._1
        }

        val n = BigInt(25489)
        var a = BigInt(8)
        var vm = BigInt(2)

        var x = a
        var y = ((a * a) - 2).mod(n)
        var gcdVMmin2_N = BigInt(1)

        var m = 1
        var steps = 0

        while (gcdVMmin2_N == 1) {
            vm = getVM(n, a, m, x, y)
            a = vm
            x = a
            y = ((a * a) - 2).mod(n)
            m += 1
            gcdVMmin2_N = (vm - 2).gcd(n).mod(n)
            steps += 1
        }

        if (gcdVMmin2_N == 0) {
            println("Not found")
            println("Steps: " + steps)
        } else {
            println("Result: " + gcdVMmin2_N + ", " + n / gcdVMmin2_N)
            println("Steps: " + steps)
        }
    }
}
