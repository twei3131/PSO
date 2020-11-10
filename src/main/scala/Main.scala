import java.io.File

import breeze.linalg.{DenseMatrix, DenseVector, argmin}
import breeze.plot.{Figure, plot}
import org.apache.commons.io.FileUtils

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set
import scala.util.Random

object Main {
  val dim: Int = 48 // 变量个数
  val popSize: Int = 1E3.toInt // 种群规模
  val resloveNumber = 1 // 解的个数
  val C1: Double = 2.8 // 个体因子
  val C2: Double = 1.3 // 社会因子
  val w: Double = 0.5 // 惯性系数
  val count: Int = 3E4.toInt //迭代次数

  // 读取数据集
  var map: DenseMatrix[Double] = read()

  def main(args: Array[String]): Unit = {
    // 初始化位置矩阵,并对位置矩阵中的元素进行限制
    var pos: DenseMatrix[Double] = DenseMatrix.zeros(popSize, dim)
    for (i <- 0 until pos.rows){
      val arr = genDifferentArray(bound = dim - 1)
      for (j <- 0 until dim){
        pos(i, j) = arr(j)
      }
    }

    // 初始化速度矩阵
    var vector: DenseMatrix[Double] = DenseMatrix.rand(popSize, dim)
    vector = vector * 2.0

    // 初始化个体最佳位置
    var pBestPos: DenseMatrix[Double] = pos

    // 个体适应度值的计算
    var fitness: DenseMatrix[Double] = DenseMatrix.zeros(popSize, resloveNumber)
    fitness = calculateFitness(pos)

    // 初始化全局最佳位置
    var gBestPos = pos(argmin(fitness)._1, ::)

    // 初始化全局和个体的最佳适应度值
    var pBestFitness: DenseMatrix[Double] = fitness
    var gBestFitness: Double = fitness(argmin(fitness))

    // 存储数据
    var data = new ArrayBuffer[Double]

    // 粒子群迭代
    for (i <- 0 until count){
      //计算速度
      var gBestPosArray: DenseMatrix[Double] = DenseMatrix.zeros(popSize, dim)
      for (j <- 0 until popSize)
        for (k <- 0 until dim)
          gBestPosArray(j, k) = gBestPos(k)
      vector = w * vector + C1 * Random.nextDouble() * (pBestPos - pos) + C2 * Random.nextDouble() * (gBestPosArray - pos)
      // 计算位置
      pos += vector

      //校正粒子群位置让其符合约束条件
      for (j <- 0 until pos.rows){
        for (k <- 0 until dim){
          pos(j,k) = pos(j,k).round
          if (pos(j,k) < 0) pos(j,k) = 0.0
          if (pos(j,k) >= dim) pos(j,k) = dim - 1
        }
        var set = Set[Double]()
        for (k <- 0 until dim) set.add(pos(j,k)) // 现有集合
        val totalSet = Set[Double]()
        for (k <- 0 until dim) totalSet.add(k) // 全集
        val diffSet = totalSet -- set // 求补集
        for (k <- 0 until dim)
          for (e <- k + 1 until dim)
            if (pos(j,e) == pos(j,k)) {
              // 去重
              val item: Double = diffSet.toVector(Random.nextInt(diffSet.size))
              pos(j, e) = item
              diffSet.remove(item)
            }
      }

      //更新个体最佳位置和适应度值,对不需要变更的进行变异操作
      var tmpPersonalFitness: DenseMatrix[Double] = calculateFitness(pos)
      val diff: DenseMatrix[Double] = pBestFitness - tmpPersonalFitness
      for (j <- 0 until diff.rows){
        if (diff(j, 0) > 0){
          pBestFitness(j, 0) = tmpPersonalFitness(j, 0)
          for (k <- 0 until dim)
            pBestPos(j, k) = pos(j, k)
        }
        // 变异操作
        val rnd = Random.nextInt(dim) // 随机选一个数
        var tmpArrayBuffer = ArrayBuffer[Double]()
        for (k <- 0 until dim)
          tmpArrayBuffer.addOne(pBestPos(j, k))
        val oldFitness = calculatePersonFitness(tmpArrayBuffer)
        var t: Double = tmpArrayBuffer(0)
        tmpArrayBuffer(0) = tmpArrayBuffer(rnd)
        tmpArrayBuffer(rnd) = t
        val newFitness = calculatePersonFitness(tmpArrayBuffer)
        if (newFitness > oldFitness) {
          for (k <- 0 until dim) {
            pos(j, k) = tmpArrayBuffer(k)
            pBestPos(j, k) = tmpArrayBuffer(k)
          }
          tmpPersonalFitness(j, 0) = newFitness
          pBestFitness(j, 0) = newFitness
        }
      }

      //更新全局最佳位置和适应度值
      gBestPos = pos(argmin(tmpPersonalFitness)._1, ::)
      gBestFitness = tmpPersonalFitness(argmin(tmpPersonalFitness))
      //变更后对全局最优进行交叉和变异

      data.addOne(gBestFitness)

      log(gBestPos.toString)
      log(gBestFitness.toString)
    }

    val f = Figure()  //在一个对话框下画一个图
    val p = f.subplot(0)
    p.title = "粒子群算法"

    var x: DenseVector[Double] = DenseVector.tabulate(data.length)(i => i)
    var y: DenseVector[Double] = DenseVector.tabulate(data.length)(i => data(i))

    p += plot(x, y)
    p.xlabel = "迭代次数"
    p.ylabel = "适应度值"

    log("")
    log(gBestFitness.toString)
    log(gBestPos.toString)
  }

  // 数学模型
  def calculateFitness(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    var sum = DenseMatrix.zeros[Double](popSize, resloveNumber)
    for (i <- 0 until matrix.rows){
      var total: Double = 0.0
      for (j <- 0 until dim - 1)
        total += map(matrix(i,j).toInt, matrix(i,j+1).toInt)
      sum(i,0) = total
    }
    return sum
  }

  // 计算标量的适应度值
  def calculatePersonFitness(vector: ArrayBuffer[Double]): Double = {
    var total: Double = 0.0
    for (j <- 0 until dim - 1)
      total += map(vector(j).toInt, vector(j+1).toInt)
    return total
  }

  //生成一组0到bound的一组不重复的正整数
  def genDifferentArray(bound: Int): List[Int] = {
    var arr= new Array[Int](bound + 1)
    for (i <- 0 to bound) arr(i) = i
    var outList: List[Int] = Nil
    var border = arr.length//随机数范围
    for(i <- 0 until bound + 1){
      //生成n个数
      val index = (new Random).nextInt(border)
      outList = outList:::List(arr(index))
      arr(index) = arr.last//将最后一个元素换到刚取走的位置
      arr = arr.dropRight(1)//去除最后一个元素
      border -= 1
    }
    return outList
  }

  // 载入数据集
  def read(): DenseMatrix[Double] = {
    var data = new DataUtil("C:\\Users\\taowei\\IdeaProjects\\PSO\\src\\main\\resources\\att48.tsp")
    var t = data.read()
    var matrix: DenseMatrix[Double] = data.convertDataSetToMatrix(t)
    return matrix
  }

  // 记录日志
  def log(str: String): Unit = {
    var file: File = new File("C:\\Users\\taowei\\IdeaProjects\\PSO\\src\\main\\resources\\log.txt")
    if (!file.exists()) file.createNewFile()
    FileUtils.writeStringToFile(file, str + "\r\n", "UTF-8", true)
  }
}
