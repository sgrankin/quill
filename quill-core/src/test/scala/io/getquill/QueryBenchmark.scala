package io.getquill

import org.scalameter.api._
import io.getquill.testContext._

object QueryBenchmark extends Benchmark {
  case class Entity(s: String)

  performance of "deeply nested queries" config (
    exec.minWarmupRuns -> 5,
    exec.maxWarmupRuns -> 5,
    exec.benchRuns -> 5,
    exec.independentSamples -> 5
  ) in {
      val sizes = Gen.exponential("size")(1, 10000, 10)
      val queries = sizes.map(mkQuery)
      using(queries) in { q =>
        run(q).string
      }
    }

  def mkQuery(size: Int) = {
    quote {
      query[Entity].filter(r => mkFilter(size).apply(r.s))
    }
  }

  def mkFilter(size: Int): Quoted[String => Boolean] = {
    if (size <= 1) quote { (p: String) =>
      infix"($p = ${lift("x")})".as[Boolean]
    }
    else { (p: String) =>
      infix"(${mkFilter(size / 2).apply(p)} OR ${mkFilter(size / 2).apply(p)})"
        .as[Boolean]
    }
  }
}
