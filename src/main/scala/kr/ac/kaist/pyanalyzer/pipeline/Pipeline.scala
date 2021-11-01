package kr.ac.kaist.pyanalyzer.pipeline

trait Pipeline[I, O]{
  def execute(input: I): O
}

object Pipeline {
  def run[I, O](p: Pipeline[I, O]): I => O = (i => p.execute(i))

  def idPipe[I]: Pipeline[I, I] =
    new Pipeline[I, I] {
      def execute(i: I): I = i  
    }

  def serial[A, B, C]
  (p1: Pipeline[A, B])
  (p2: Pipeline[B, C]): Pipeline[A, C] =
    new Pipeline[A, C] {
      def execute(i: A): C = p2.execute(p1.execute(i))
    }

  def parallel[I, A, B]
  (p1: Pipeline[I, A])
  (p2: Pipeline[I, B]): Pipeline[I, (A, B)] =
    new Pipeline[I, (A, B)] {
      def execute(i: I): (A, B) = (p1.execute(i), p2.execute(i))
    }

  def append[I, A, B]
  (p1: Pipeline[I, A])
  (p2: Pipeline[A, B]): Pipeline[I, (A, B)] =
    new Pipeline[I, (A, B)] {
      def execute(i: I): (A, B) = {
        val resA = p1.execute(i)
        val resB = p2.execute(resA)
        (resA, resB)
      }
    }

  implicit class PipelinePairs[I, A, B](p: Pipeline[I, (A, B)]) {
    def fst: Pipeline[I, A] = 
      new Pipeline[I, A] {
        def execute(i: I): A = p.execute(i)._1     
      }
    def snd: Pipeline[I, B] =
      new Pipeline[I, B] {
        def execute(i: I): B = p.execute(i)._2     
      }
    def fstMap[X](p2: Pipeline[A, X]): Pipeline[I, (X, B)] =
      new Pipeline[I, (X, B)] {
        def execute(i: I): (X, B) = {
          val (resA, resB) = p.execute(i)
          val resX = p2.execute(resA)
          (resX, resB)
        }
      }
  }

  implicit class PipelineTriple[I, O](p: Pipeline[I, O]) {
    def parallel3[A, B, C](
    pA: Pipeline[O, A], 
    pB: Pipeline[O, B],
    pC: Pipeline[O, C]): Pipeline[I, (A, B, C)] = 
      new Pipeline[I, (A, B, C)] {
        def execute(i: I): (A, B, C) = {
          val resO = p.execute(i)
          (pA.execute(resO), pB.execute(resO), pC.execute(resO)) 
        } 
      }
  }

  implicit class PipelineOps[I, O](p: Pipeline[I, O]) {
    def !!(input: I): O = run(p)(input)
    def >>>[T](f: Pipeline[I, O] => T): T = f(p)
    def >>[O2](p2: Pipeline[O, O2]): Pipeline[I, O2] = serial(p)(p2)
    def ||[O2](p2: Pipeline[I, O2]): Pipeline[I, (O, O2)] = parallel(p)(p2)
    def ++[O2](p2: Pipeline[O, O2]): Pipeline[I, (O, O2)] = append(p)(p2)
  }
}
