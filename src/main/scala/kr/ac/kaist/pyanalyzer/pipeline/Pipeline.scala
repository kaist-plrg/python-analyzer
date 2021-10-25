package kr.ac.kaist.pyanalyzer.pipeline

trait Pipeline[I, O]{
  def execute(input: I): O
}

object Pipeline {
  def run[I, O](p: Pipeline[I, O]): I => O = (i => p.execute(i))

  def serial[A, B, C](p1: Pipeline[A, B])(p2: Pipeline[B, C]): Pipeline[A, C] =
    new Pipeline[A, C] {
      def execute(i: A): C = p2.execute(p1.execute(i))
    }

  def parallel[I, A, B](p1: Pipeline[I, A])(p2: Pipeline[I, B]): Pipeline[I, (A, B)] =
    new Pipeline[I, (A, B)] {
      def execute(i: I): (A, B) = (p1.execute(i), p2.execute(i))
    }

  implicit class PipelineOps[I, O](p: Pipeline[I, O]) {
    def !!(input: I): O = run(p)(input)
    def >>[O2](other: Pipeline[O, O2]): Pipeline[I, O2] = serial(p)(other)
    def ||[O2](other: Pipeline[I, O2]): Pipeline[I, (O, O2)] = parallel(p)(other)
  }
}
