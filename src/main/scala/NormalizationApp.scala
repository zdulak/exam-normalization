import scopt.OParser
import scala.math._
import java.io.File
import scala.io.Source
import java.io._

object NormalizationApp {
  def main(args: Array[String]): Unit = {
    OParser.parse(getParser, args, Config()) match {
      case Some(config) => {
        val normalizedData = getNormalizedData(config.filename)
        writeToFile(normalizedData, config.filename)
      }
      case None => println("Invalid configuration")
    }
  }

  def writeToFile(data: Iterator[String], filename: String): Unit = {
    val fileWriter = new FileWriter("normalized-" + filename)
    data.foreach(line => {
      fileWriter.write(line  + "\n")
    })
    fileWriter.close()
  }

  def getNormalizedData(fileName: String): Iterator[String] = {
    def getKeyValue(line: String) = {
      val elems = line.split(",")
      (elems(0), elems(1).toDouble)
    }

    val (minValue, maxValue) =
      Source.fromFile(fileName).getLines().foldLeft((Double.MaxValue, Double.MinValue)) { case (minMax, line) =>
        val value = getKeyValue(line)._2
//      first is minValue, second is maxValue
        (min(minMax._1, value), max(minMax._2, value))
    }
//    We have to use Source.fromFile (fileName) again because otherwise the iterator is empty
    Source.fromFile(fileName).getLines().map { line =>
      val (key, value) = getKeyValue(line)
      key + "," + normalize(value, minValue, maxValue).toString
    }
  }

  def normalize(value: Double, minValue: Double, maxValue: Double): Double = {
    (2 * (value - minValue) / (maxValue - minValue)) - 1
  }

  private def getParser: OParser[String, Config] = {
    val builder = OParser.builder[Config]
    val parser = {
      import builder._
      OParser.sequence(
        arg[String]("<file name>")
          .required()
          .validate(name => if (new File(name).exists()) success else failure("File does not exist"))
          .action {(name, config) => config.copy(filename = name)}
          .text("Name of the file"),
        help("help").text("prints this usage text")
      )
    }
    parser
  }
}
