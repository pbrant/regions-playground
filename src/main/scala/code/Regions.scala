package code

import scalaz._
import Scalaz._
import scalaz.effect._
import scalaz.effect.RegionT._
import scala.language.higherKinds

import Kleisli._

import java.io.{BufferedReader, FileReader}

object Regions {
  class FileLineReader[R[_]](in: BufferedReader) {
    def readLine(): Option[String] = Option(in.readLine())
  }

  object FileLineReader {
    def open[S,P[_]: MonadIO](path: String): RegionT[S,P,FileLineReader[({type l[a] = RegionT[S,P,a]})#l]] = {
      val F = Monad[({type l[a] = RegionT[S, P, a]})#l]

      for {
        buf <- F.point(IOUtil.openFile(path))
        handle <- IO.onExit(IO(IOUtil.close(path, buf)))
      } yield {
        new FileLineReader[({type l[a] = RegionT[S,P,a]})#l](buf)
      }
    }

    def read[S,P[_]: MonadIO](resource: FileLineReader[({type l[a] = RegionT[S,P,a]})#l]): RegionT[S,P,Option[String]] =
      IO(resource.readLine()).liftIO[({type l[a] = RegionT[S,P,a]})#l]

    def all[S,P[_]: MonadIO,G[_]](
      resource: FileLineReader[({type l[a] = RegionT[S,P,a]})#l]
    )(
      implicit G: ApplicativePlus[G] with Traverse[G]
    ): RegionT[S,P,G[String]] = {
      def loop(g: G[String]): IO[G[String]] =
        IO(resource.readLine()).flatMap(o =>
          o.map(s => loop(G.plus(G.point(s), g))).getOrElse(IO(g))
        )
      loop(G.empty).map(G.reverse).liftIO[({type l[a] = RegionT[S,P,a]})#l]
    }
  }
}
