package subMatch

import java.io.File
import util.matching.Regex
import tools.Levenshtein


/**
 * Rename subtitles to match movies
 * User: vans
 * Date: 03/10/13
 * Time: 21:29
 * To change this template use File | Settings | File Templates.
 */
object Console {

  def listFiles(directory : File, filter : String) = {
    directory.listFiles().filter(_.getName.endsWith(filter))
  }

  def fileMatchScore(s : String, srtName : String) = Levenshtein.distance(s, srtName)

  def extractNumbers(s : String) = {
    val numbers = new Regex("([\\d]+)")
    numbers.findAllMatchIn(s).flatMap(m => m.matched).mkString("")
  }

  def bestFileMatch(s1: String, s2: String, videoName: String): String = {
    val numS1 = extractNumbers(s1)
    val numS2 = extractNumbers(s2)
    val numVideo = extractNumbers(videoName)
    val s1Score = fileMatchScore(numS1, numVideo)
    val s2Score = fileMatchScore(numS2, numVideo)
    if (s1Score <= s2Score)
      s1
    else
      s2
  }

  def findVideoMatch(videoName : String, strNames : Seq[String]) : String =
    (strNames :\ "") ((current, accu) =>
      bestFileMatch(current, accu, videoName))

  /**
   *
   * @param args subtitles path
   */
  def main(args: Array[String]) {
    val path = args(0)
    val listSub = listFiles(new File(path), ".srt")
    val listMovies = listFiles(new File(path), ".avi") ++ listFiles(new File(path), ".mp4")
    for (s <- listMovies) {
      println("srt : " + s.getName + " ===> " + findVideoMatch(s.getName, listSub.map(_.getName)))
    }
  }
}
