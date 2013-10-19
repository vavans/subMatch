package subMatch

import java.io.File
import scala.util.matching.Regex


/**
 * Created with IntelliJ IDEA.
 * User: vans
 * Date: 03/10/13
 * Time: 21:29
 * To change this template use File | Settings | File Templates.
 */
object Console {

  def listFiles(directory : File, filter : String) = directory.listFiles().filter(_.getName.endsWith(filter))

  def fileMatchScore(s : String, srtName : String) = {
    def findNumbers(s : String) = {
      val numbers = new Regex("([\\d]+)")
      numbers.findAllIn(s)
    }

    val sNumbers = findNumbers(s)
    val strNameNumber = findNumbers(srtName)

    sNumbers.count(strNameNumber.contains(_))
  }

  def bestFileMatch(s1: String, s2: String, srtName: String): String = {
    val s1Score = fileMatchScore(s1, srtName)
    val s2Score = fileMatchScore(s2, srtName)
    if (s1Score > s2Score)
      s1
    else
      s2
  }

  def findVideoMatch(srtName : String, videoNames : Seq[String]) : String =
    (videoNames :\ "") ((current, accu) =>
      bestFileMatch(current, accu, srtName))

  /**
   *
   * @param args subtitles path
   */
  def main(args: Array[String]) {
    println("Hello, world!")
    println(new File(args(0)).getAbsolutePath)
    val listSub = listFiles(new File(args(0)), ".srt")
    val listAvi = listFiles(new File(args(0)), ".avi")
    for (s <- listSub) {
      println("srt : " + s.getName + " ===> " + findVideoMatch(s.getName, listAvi.map(_.getName)))
      println(s.getName)
    }
  }
}
