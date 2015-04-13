package code

import scalaz._
import Scalaz._
import scalaz.effect._
import scala.language.higherKinds

import Kleisli._

import java.io.{BufferedReader, FileReader}

object IORegions {
  trait SafeResource[R[_]]

  type ResourceFinalizer = IO[Unit]
  type FinalizerRefs = IORef[List[ResourceFinalizer]]
  type ResourceReader[A] = ReaderT[IO,IORef[List[ResourceFinalizer]],A]
  type ForallRegion[A] = Forall[({type f[s] = IORegion[s,A]})#f]

  case class IORegion[S,A](value: ResourceReader[A]) {
    def flatMap[B](f: A => IORegion[S,B]): IORegion[S,B] =
      IORegion[S,B](kleisli(s => this.value.run(s) flatMap (a => f(a).value.run(s))))

    def map[B](f: A => B): IORegion[S,B] =
      IORegion[S,B](this.value.map(f))
  }

  trait IORegionInstances {
    implicit def ioRegionMonad[S]: Monad[({type f[a] = IORegion[S,a]})#f] =
      new IORegionMonad[S] {}

    trait IORegionMonad[S] extends Monad[({type f[a] = IORegion[S,a]})#f] {
      override def bind[A,B](fa: IORegion[S,A])(f: A => IORegion[S,B]): IORegion[S,B] =
        fa flatMap f

      override def point[A](a: => A): IORegion[S,A] =
        IORegion[S,A](kleisli(_ => IO(a)))
    }

    // LiftIO and MonadIO needed for IO(...).liftIO[IORegion] (which don't add much for us [yet?])

    implicit def ioRegionLiftIO[S]: LiftIO[({type f[a] = IORegion[S,a]})#f] =
      new IORegionLiftIO[S] {}

    trait IORegionLiftIO[S] extends LiftIO[({type f[a] = IORegion[S,a]})#f] {
      def liftIO[A](ioa: IO[A]) = IORegion[S,A](kleisli(_ => ioa))
    }

    implicit def ioRegionMonadIO[S]: MonadIO[({type f[a] = IORegion[S,a]})#f] =
      new MonadIO[({type f[a] = IORegion[S, a]})#f] with IORegionLiftIO[S] with IORegionMonad[S] {
        implicit def M = MonadIO[IO]
        implicit def L = MonadIO[IO]
      }
  }

  trait IORegionFunctions {
    // def liftIO[S,A](io: IO[A]): IORegion[S,A] = IORegion[S,A](kleisli(_ => io))
    def liftIO[S,A](io: IO[A]): IORegion[S,A] = io.liftIO[({type f[a] = IORegion[S,a]})#f]

    def runRegion[A](f: Forall[({type f[s] = IORegion[s,A]})#f]): IO[A] =
      IO.newIORef(List[ResourceFinalizer]()).bracket(
        _.read.flatMap(_.sequence)
      )(
        f.apply.value.run(_)
      )
  }

  object IORegion extends IORegionFunctions with IORegionInstances

  class FileLineReader[R[_]](in: BufferedReader) extends SafeResource[R] {
    def readLine(): Option[String] = Option(in.readLine())
  }

  object FileLineReader {
    import IORegion.liftIO

    def open[S](path: String): IORegion[S,FileLineReader[({type f[a] = IORegion[S,a]})#f]] = {
      IORegion[S,FileLineReader[({type f[a] = IORegion[S,a]})#f]](
        for {
          r <- IO(IOUtil.openFile(path)).liftIO[ResourceReader]
          refs <- ask[IO,FinalizerRefs]
          _ <- refs.mod(IO(IOUtil.close(path, r)) :: _).liftIO[ResourceReader]
        } yield new FileLineReader[({type f[a] = IORegion[S,a]})#f](r)
      )
    }

    def read[S](resource: FileLineReader[({type f[a] = IORegion[S,a]})#f]): IORegion[S,Option[String]] =
      liftIO[S,Option[String]](IO(resource.readLine()))

    def all[S,G[_]](
      resource: FileLineReader[({type f[a] = IORegion[S,a]})#f]
    )(
      implicit G: ApplicativePlus[G] with Traverse[G]
    ): IORegion[S,G[String]] = {
      def loop(g: G[String]): IO[G[String]] =
        IO(resource.readLine()).flatMap(o =>
          o.map(s => loop(G.plus(G.point(s), g))).getOrElse(IO(g))
        )
      liftIO[S,G[String]](loop(G.empty).map(G.reverse))
    }
  }
}
