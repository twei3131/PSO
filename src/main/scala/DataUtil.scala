import breeze.linalg.DenseMatrix

import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}
import scala.util.control.Breaks.{break, breakable}

class DataUtil(filePath: String) {

  //读取数据集,返回的是ArrayBuffer
  def read(): ArrayBuffer[Map[String, String]] = {
    var arrayBuffer = ArrayBuffer[Map[String, String]]()
    var lines: BufferedSource = Source.fromFile(filePath)
    var index = 0
    for (line <- lines.getLines()){
      breakable{
        index += 1
        if (index <= 6) break
        if (line == "EOF") break()

        var array: Array[String] = line.split(" ")
        arrayBuffer.addOne(Map("name" -> array(0), "x" -> array(1), "y" -> array(2)))
      }
    }
    return arrayBuffer
  }

  //将数据集转化为二维矩阵，方便求解
  def convertDataSetToMatrix(dataSet: ArrayBuffer[Map[String, String]]): DenseMatrix[Double] = {
    var matrix = DenseMatrix.zeros[Double](dataSet.length, dataSet.length)
    for (i <- 0 until dataSet.length){
      for (j <- 0 until dataSet.length){
        breakable{
          if (i == j || matrix(i, j) != 0) break //对角线元素应为0, 两个城市之间的距离大于零
          val xDiff: Double = dataSet(i).get("x").get.toDouble - dataSet(j).get("x").get.toDouble
          val yDiff: Double = dataSet(i).get("y").get.toDouble - dataSet(j).get("y").get.toDouble
          matrix(i, j) = math.sqrt(math.pow(xDiff, 2) + math.pow(yDiff, 2)) //计算两点之间欧氏距离
          matrix(j, i) = matrix(i, j) //无向图
        }
      }
    }
    return matrix
  }
}
