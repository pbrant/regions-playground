package code

import scalaz._
import Scalaz._
import scalaz.effect._
import scala.language.higherKinds

import Kleisli._

object regions extends App {
  case class Handle(path: String)
  case class SHandle[R[_]](value: Handle)

  case class IORegion[S,A](value: ReaderT[IO,IORef[List[Handle]],A]) {
    def flatMap[B](f: A => IORegion[S,B]): IORegion[S,B] =
      IORegion[S,B](kleisli(s => this.value.run(s) flatMap (a => f(a).value.run(s))))

    def map[B](f: A => B): IORegion[S,B] =
      IORegion[S,B](this.value.map(f))
  }

  trait IORegionInstances {
    implicit def regionTMonad[S]: Monad[({type f[a] = IORegion[S,a]})#f] =
      new Monad[({type f[a] = IORegion[S,a]})#f] {
        override def bind[A,B](fa: IORegion[S,A])(f: A => IORegion[S,B]): IORegion[S,B] =
          fa flatMap f

        override def point[A](a: => A): IORegion[S,A] =
          IORegion[S,A](kleisli(_ => IO(a)))
      }
  }

  trait IORegionFunctions {
    def liftIO[S,A](io: IO[A]): IORegion[S,A] =
      IORegion[S,A](kleisli(_ => io))

    def open[S](path: String): IORegion[S,SHandle[({type f[a] = IORegion[S,a]})#f]] = {
      type XIOReaderT[A] = ReaderT[IO,IORef[List[Handle]],A]
      IORegion[S,SHandle[({type f[a] = IORegion[S,a]})#f]](
        for {
          h <- IO(Handle(path)).liftIO[XIOReaderT]
          handles <- Kleisli.ask[IO,IORef[List[Handle]]]
        } yield SHandle[({type f[a] = IORegion[S,a]})#f](h)
      )
    }

    def read[S](handle: SHandle[({type f[a] = IORegion[S,a]})#f]): IORegion[S,String] =
      liftIO[S,String](IO("read a line!"))

    def runRegion[A](f: Forall[({type f[s] = IORegion[s,A]})#f]): IO[A] =
      for {
        handles <- IO.newIORef(List[Handle]())
        result <- f.apply.value.run(handles)
      } yield result
  }

  object IORegion extends IORegionFunctions with IORegionInstances

  def test1[S]: IORegion[S,String] =
    for {
      handle <- IORegion.open("/tmp/foo.txt")
      line <- IORegion.read(handle)
    } yield line

  def forall1 = new Forall[({type f[s] = IORegion[s,String]})#f] {
    def apply[A] = test1
  }

  val h = IORegion.runRegion(forall1)

  println(h.unsafePerformIO())
}

