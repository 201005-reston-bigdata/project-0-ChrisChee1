package proj0

import scala.io.{BufferedSource, Source}

object FileUtil {

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
