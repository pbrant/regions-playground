package code

import scalaz._
import Scalaz._

import scalaz.effect._

import scala.language.higherKinds

import RegionT._
import RefCountedFinalizer._
import FinalizerHandle._

import Kleisli._

object RegionsApp extends App {
  import Regions._

  def test1[S]: RegionT[S,IO,List[String]] = {
    for {
      handle <- IO.onExit[S,IO](IO(println("Exiting region (1)...")))
      file <- FileLineReader.open[S,IO]("/tmp/foo.txt")
      handle <- IO.onExit[S,IO](IO(println("Exiting region (2)...")))
      lines <- FileLineReader.all[S,IO,List](file)
    } yield lines
  }

  def forall1 = new Forall[({type l[a] = RegionT[a,IO,List[String]]})#l] {
    def apply[S] = test1
  }

  val M = Monad[IO]

  IO.runIORegion[List[String]](forall1)
    .map(println(_))
    .unsafePerformIO()

  /*
  def runRegionT[A](r: Forall[({type λ[S] = RegionT[S, IO, A]})#λ]): IO[A] = {
    def after(hsIORef: IORef[List[RefCountedFinalizer]]): IO[Unit] = {
      for {
        hs <- hsIORef.read
        _ <- hs.traverse[IO,Unit](r => for {
          refCnt <- r.refcount.mod(_ - 1)
          _ <- if (refCnt == 0) r.finalizer else IO.ioUnit
        } yield ())
      } yield ()
    }
    // Works
    IO.newIORef(List[RefCountedFinalizer]()).bracket(after)(s => r.apply.value.run(s))

    // Doesn't work
    // IO.newIORef(List[RefCountedFinalizer]()).bracketIO(after)(s => r.apply.value.run(s))

    // Doesn't work (inlined version of real runRegionT[,IO,]))
    // val f = (runInIO: IO.RunInBase[IO,IO]) =>
    //   IO.newIORef(List[RefCountedFinalizer]()).bracket(after)(
    //     runInIO.apply compose (s => r.apply.value.run(s)))
    // Monad[IO].join(ioLiftControl(f))

    // Works (add `ensuring` to M[B], which we know is IO here)
    // val f = () =>
    //   IO.newIORef(List[RefCountedFinalizer]()).bracket(after)(
    //       s => IO(r.apply.value.run(s).ensuring(after(s))))
    // Monad[IO].join(f())
  }
  */

  def ioLiftControl[A](f: IO.RunInBase[IO, IO] => IO[A]): IO[A] =
    f(new IO.RunInBase[IO, IO] {
        def apply[B]: IO[B] => IO[IO[B]] = (x: IO[B]) => IO(x)
    })

  def controlIO[M[_], A](f: IO.RunInBase[M, IO] => IO[M[A]])(implicit M: MonadControlIO[M]): M[A] =
    M.join(M.liftControlIO(f))

  def bracketIO[M[_], A, B](self: IO[A])(after: A => IO[Unit])(during: A => M[B])(implicit m: MonadControlIO[M]): M[B] =
    controlIO((runInIO: IO.RunInBase[M, IO]) => self.bracket(after)(runInIO.apply compose during))
}
