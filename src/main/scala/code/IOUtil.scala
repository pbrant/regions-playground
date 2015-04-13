package code

import java.io.{BufferedReader, FileReader}

object IOUtil {
  def openFile(path: String): BufferedReader = {
    println("Opening file " + path)
    new BufferedReader(new FileReader(path))
  }

  def close(path: String, in: BufferedReader): Unit = {
    try {
      println("Closing file " + path)
      in.close()
    } catch {
      case e: Throwable => // ignore
    }
  }
}
