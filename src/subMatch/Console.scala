package subMatch

import java.io.File
import scala.collection.generic.FilterMonadic

/**
 * Created with IntelliJ IDEA.
 * User: vans
 * Date: 03/10/13
 * Time: 21:29
 * To change this template use File | Settings | File Templates.
 */
object Console {

  def listFiles(directory : File, filter : String) = directory.listFiles().filter(_.getName.endsWith(filter))

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
      println(s.getName)
    }
  }
}