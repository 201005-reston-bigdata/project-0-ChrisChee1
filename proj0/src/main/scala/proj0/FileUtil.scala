package proj0

import scala.io.{BufferedSource, Source}

/** Static FileUtil class to extract text from files */
object FileUtil {

  /** Returns formatted text string extracted from a CSV file */
  def getCSVText(fileName: String): Option[String] =
  {
    var openFile : BufferedSource = null

    try
    {
      openFile = Source.fromFile(fileName)
      Some(openFile.getLines().mkString("\n"))
    }
    finally
    {
      if (openFile != null)
        openFile.close()
    }
  }

  /** Returns formatted text string extracted from a JSON file */
  def getJSONText(fileName: String): Option[String] =
  {
    var openFile : BufferedSource = null

    try
    {
      openFile = Source.fromFile(fileName)
      Some(openFile.getLines().mkString(""))
    }
    finally
    {
      if (openFile != null)
        openFile.close()
    }
  }
}