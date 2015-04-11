package code

import scalaz._
import Scalaz._

object IORegionApp extends App {
  import IORegions._

  def test1[S]: IORegion[S,List[String]] = {
    for {
      file <- FileLineReader.open("/tmp/foo.txt")
      lines <- FileLineReader.all[S,List](file)
    } yield lines
  }

  def forall1 = new ForallRegion[List[String]] {
    def apply[S] = test1
  }

  val h = IORegion.runRegion(forall1)

  println(h.unsafePerformIO())
}
