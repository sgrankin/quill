package io.getquill

import org.scalameter.api._

abstract class Benchmark extends Bench.ForkedTime {
  override def measurer = new Measurer.Default
}
