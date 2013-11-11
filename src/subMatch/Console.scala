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

  def extractNumbers(s : String) = s
  /*{
    val numbers = new Regex("([\\d]+)")
    numbers.findAllMatchIn(s).flatMap(m => m.matched).mkString("")
  }*/

  def bestFileMatch(s1: File, s2: File, videoName: String): File = {
    val numS1 = extractNumbers(s1.getName)
    val numS2 = extractNumbers(s2.getName)
    val numVideo = extractNumbers(videoName)
    val s1Score = fileMatchScore(numS1, numVideo)
    val s2Score = fileMatchScore(numS2, numVideo)
    if (s1Score <= s2Score)
      s1
    else
      s2
  }

  def findVideoMatch(videoName : String, strNames : Seq[File]) : File =
    (strNames :\ new File("")) ((current, accu) =>
      bestFileMatch(current, accu, videoName))

  /**
   *
   * @param args subtitles path
   */
  def main(args: Array[String]) {
    val path = args(0)
    val listSub = listFiles(new File(path), ".srt")
    val listMovies = listFiles(new File(path), ".avi") ++ listFiles(new File(path), ".mp4")
    for (movie <- listMovies) {
      val sub = findVideoMatch(movie.getName, listSub)
      println("srt : " + movie.getName + " ===> " + sub.getName)
      val newSub = new File(movie.getAbsolutePath.dropRight(4) + ".srt")
      sub.renameTo(newSub)
    }
  }
}
