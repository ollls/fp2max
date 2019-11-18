package example

import scala.io.StdIn.readLine

object App1 {

  case class IO[A](unsafeRun: () => A) { self =>
    def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(self.unsafeRun()).unsafeRun())
  }

  trait Program[F[_]] {
    def finish[A](a: => A): F[A]
    def chain[A, B](fa: F[A], afb: A => F[B]): F[B]
    def map[A, B](fa: F[A], ab: A => B): F[B]
  }

  //implicitly[Console[F]].putStrLn("Hello World!")

  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit F: Program[F]): F[B] = F.map(fa, f)
    def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] =
      F.chain(fa, afb)
  }

  trait Console[F[_]] {
    def putStrLn(line: String): F[Unit]
    def getStrLn: F[String]
  }

  trait Random[F[_]] {
    def nextInt(upper: Int): F[Int]
  }

  object Console {
    def apply[F[_]](implicit con: Console[F]): Console[F] = con
  }

  object Random {
    def apply[F[_]](implicit rnd: Random[F]): Random[F] = rnd
  }

  object IO {

    def unit[A](a: => A): IO[A] = IO(() => a)

    implicit val ProgramIO = new Program[IO] {
      def finish[A](a: => A): IO[A] = IO.unit(a)
      def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] = fa.flatMap(afb)
      def map[A, B](fa: IO[A], ab: A => B): IO[B] = fa.map(ab)
    }

    implicit val ConsoleIO = new Console[IO] {
      def putStrLn(line: String): IO[Unit] = IO(() => println(line))
      def getStrLn: IO[String] = IO(() => readLine())
    }

    implicit val RandomIO = new Random[IO] {
      def nextInt(upper: Int): IO[Int] =
        IO(() => scala.util.Random.nextInt(upper))
    }

    def runMe[F[_]: Console: Program: Random](name: String): F[Unit] = {
      for {
        line <- Console[F].getStrLn
        rnd <- Random[F].nextInt(10).map(c => c + 1)
        _ <- Console[F].putStrLn(line)
      } yield ()

    }

    def runMeIO(name: String): IO[Unit] = runMe[IO](name)

  }
}

object Hello extends Greeting with App {
  println(greeting)
}

trait Greeting {
  lazy val greeting: String = "hello"
}
