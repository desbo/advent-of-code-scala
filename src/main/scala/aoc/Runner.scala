package aoc

import cats.Show
import cats.effect.{IO, IOApp}
import cats.syntax.all.*

import java.time.Duration

object Runner extends IOApp.Simple:
  import twentyfive._
  val year = 2025

  val solutionsByDay = Map(7 -> Day7)

  override def run: IO[Unit] =
    for
      token <- IO.fromOption(sys.env.get("AOC_TOKEN"))(new Exception("missing AOC_TOKEN env var"))
      downloader = InputDownloader.default(token)
      _ <- solveAllDays(downloader)
    yield ()

  def solveAllDays(inputDownloader: InputDownloader): IO[Unit] =
    (1 to 25)
      .flatMap: day =>
        solutionsByDay.get(day).tupleLeft(day)
      .toList
      .traverse: (day, solution) =>
        import solution.given

        for
          _     <- IO.println(s"- day $day ---------------------------------")
          input <- inputDownloader.download(year, day)
          parts <- runSolution(solution, input)
          _     <- parts.traverse(showResult)
          _     <- IO.println("")
        yield ()
      .void

  def runSolution[A](solution: Solution[_, A], input: String): IO[List[(Int, (A, Duration))]] =
    val parsed = solution.parse(input)

    def runPart(partNum: Int) =
      for
        start  <- IO.realTimeInstant
        result <- IO(if (partNum == 1) solution.part1(parsed) else solution.part2(parsed))

        end <- IO.realTimeInstant
      yield (result, Duration.between(start, end))

    List(1, 2).traverseFilter: num =>
      runPart(num)
        .map(r => (num, r).some)
        .recover:
          case _: NotImplementedError =>
            none

  def showResult[A: Show](result: (Int, (A, Duration))): IO[Unit] =
    val (part, (answer, duration)) = result
    IO.println(show"\tpart $part:") >>
      IO.println(show"\t\tanswer\t= $answer") >>
      IO.println(show"\t\truntime\t= ${duration.toNanos / 1e+6}ms")
