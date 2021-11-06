package GPU;

import com.aparapi.Kernel;

public class SimplexGPUUtils extends Kernel {

    // переменные доступные в Kernel будут скопированы в GPU RAM
    private double[] matrixGPU;
    private double[] freeColumn;
    private final int rowsCount;
    private final int columnsCount;
    private int[] basicVariables;
    private int[] variables;

    @Override
    public void run() {}

    public double[] getMatrix() {
        return this.matrixGPU;
    }

    public int getRowsCount() {
        return this.rowsCount;
    }

    public int getColumnsCount() {
        return this.columnsCount;
    }

    public int[] getBasicVariables() {
        return this.basicVariables;
    }

    public int[] getVariables() {
        return this.variables;
    }

    // получить столбец свободных членов
    public double[] getFreeColumn() {
        return this.freeColumn;
    }

    public SimplexGPUUtils(
                               double[][] matrixCPU,
                               int rowsCount,
                               int columnsCount,
                               int[] basicVariables,
                               int[] variables
                          ) {
        // обращаться к полям класса внутри run() нельзя, нужно сделать копии
        double[] matrixGPU = new double[rowsCount * columnsCount];
        double[] freeColumn = new double[rowsCount];
        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                // index = 0 ... rowsCount * columnsCount - 1
                int index = getGlobalId();
                matrixGPU[index] = matrixCPU[index / columnsCount][index % columnsCount];
                if (index % columnsCount == 0) {
                    freeColumn[index / columnsCount] = matrixGPU[index];
                }
            }
        };
        kernel.execute(rowsCount * columnsCount);
        this.matrixGPU = matrixGPU;
        this.freeColumn = freeColumn;
        this.rowsCount = rowsCount;
        this.columnsCount = columnsCount;
        this.basicVariables = basicVariables;
        this.variables = variables;
    }

    public double[] getSolution() {
        double[] freeColumn = this.freeColumn;
        int rowsCount = this.rowsCount;
        int columnsCount = this.columnsCount;
        double[] solution = new double[(rowsCount - 1) + (columnsCount - 1)];
        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                // index = 0 ... (rowsCount - 1) + (columnsCount - 1) - 1
                int index = getGlobalId();
                if (index < (rowsCount - 1)) {
                    solution[index] = freeColumn[index];
                } else {
                    solution[index] = 0.0;
                }
            }
        };
        kernel.execute((rowsCount - 1) + (columnsCount - 1));
        return solution;
    }

    public double getObjectiveFunction() {
        return this.freeColumn[this.rowsCount - 1];
    }

    public double[] getRow(int rowIndex) {
        double[] matrixGPU = this.matrixGPU;
        int columnsCount = this.columnsCount;
        double[] row = new double[columnsCount];
        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                // index = 0 ... columnsCount - 1
                int columnIndex = getGlobalId();
                row[columnIndex] = matrixGPU[rowIndex * columnsCount + columnIndex];
            }
        };
        kernel.execute(columnsCount);
        return row;
    }

    public double[] getColumn(int columnIndex) {
        double[] matrixGPU = this.matrixGPU;
        int columnsCount = this.columnsCount;
        int rowsCount = this.rowsCount;
        double[] column = new double[this.rowsCount];
        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                // index = 0 ... rowsCount - 1
                int rowIndex = getGlobalId();
                column[rowIndex] = matrixGPU[rowIndex * columnsCount + columnIndex];
            }
        };
        kernel.execute(rowsCount);
        return column;
    }

    // получить массив отношений столбцов
    public double[] getColumnsRatios(
                                        int numeratorColumnIndex,
                                        int denominatorColumnIndex,
                                        double zeroDivisionReplacement,
                                        int skipByNegativeDivision,
                                        double byNegativeDivisionReplacement
                                    ) {
        double[] numeratorArr;
        if (numeratorColumnIndex == 0) {
            numeratorArr = this.freeColumn;
        } else {
            numeratorArr = this.getColumn(numeratorColumnIndex);
        }
        double[] denominatorArr = this.getColumn(denominatorColumnIndex);
        int rowsCount = this.rowsCount;
        double[] ratios = new double[rowsCount];
        // эпсилон для сравнения числа с нулем
        double zeroEpsilon = 0.000001;
        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                // index = 0 ... rowsCount - 1
                int index = getGlobalId();
                double numerator = numeratorArr[index];
                double denominator = denominatorArr[index];
                // абсолютное значение знаменателя
                double denominatorAbs = denominator;
                if (denominatorAbs < 0.0) {
                    denominatorAbs = (-1.0) * denominatorAbs;
                }
                if (denominatorAbs <= zeroEpsilon) {
                    // деление на ноль
                    ratios[index] = zeroDivisionReplacement;
                } else if (denominator < 0.0 && skipByNegativeDivision == 1) {
                    // деление на отрицательное s_i_k
                    ratios[index] = byNegativeDivisionReplacement;
                } else {
                    ratios[index] = numerator / denominator;
                }
            }
        };
        kernel.execute(rowsCount);
        return ratios;
    }

    // смена базисных переменных
    // в arr1 под индексом index1 поместить элемент arr2[index2]
    // аналогично для arr2
    public void baseChange(int oldBaseIndex, int oldVariableIndex) {
        int[] basicVariables = this.basicVariables;
        int[] variables = this.variables;
        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                int oldBasicVariable = basicVariables[oldBaseIndex];
                basicVariables[oldBaseIndex] = variables[oldVariableIndex];
                variables[oldVariableIndex] = oldBasicVariable;
            }
        };
        kernel.execute(1);
        this.basicVariables = basicVariables;
        this.variables = variables;
    }

    public void computeJordanExceptions(int permissiveRowIndex, int permissiveColumnIndex) {
        double[] matrixGPU = this.matrixGPU;
        double[] freeColumn = this.freeColumn;
        int rowsCount = this.rowsCount;
        int columnsCount = this.columnsCount;
        Kernel kernel = new Kernel() {
            @Override
            public void run() {
                // index = 0 ... rowsCount * columnsCount - 1
                int index = getGlobalId();
                int rowIndex = index / columnsCount;
                int columnIndex = index % columnsCount;
                double permissiveRowColumnValue = matrixGPU[permissiveRowIndex * columnsCount + permissiveColumnIndex];
                // s_r_k* = 1 / s_r_k
                if (rowIndex == permissiveRowIndex && columnIndex == permissiveColumnIndex) {
                    matrixGPU[index] = 1.0 / permissiveRowColumnValue;
                }
                // s_r_j* = s_r_j / s_r_k, j = 0,...,n, j != k
                else if (rowIndex == permissiveRowIndex) {
                    matrixGPU[index] = matrixGPU[index] / permissiveRowColumnValue;
                }
                // s_i_k* = - s_i_k / s_r_k, i = 1,..., m + 1, i != r
                else if (columnIndex == permissiveColumnIndex) {
                    matrixGPU[index] = (-1.0) * (matrixGPU[index] / permissiveRowColumnValue);
                }
                // s_i_j* = s_i_j - ((s_i_k * s_r_j) / s_r_k), i = 0,..., m + 1, i != r, j = 0,..., n, j != k
                else {
                    double permissiveColumnValue = matrixGPU[rowIndex * columnsCount + permissiveColumnIndex];
                    double permissiveRowValue = matrixGPU[permissiveRowIndex * columnsCount + columnIndex];
                    matrixGPU[index] = matrixGPU[index] - ( (permissiveColumnValue * permissiveRowValue) / permissiveRowColumnValue );
                }
                if (columnIndex == 0) {
                    freeColumn[rowIndex] = matrixGPU[index];
                }
            }
        };
        kernel.execute(rowsCount * columnsCount);
        this.matrixGPU = matrixGPU;
        this.freeColumn = freeColumn;
    }
}
