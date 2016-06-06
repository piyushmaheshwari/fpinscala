trait Generator[+T] {
  self =>
  // an alias for "this"

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new java.util.Random()
  def generate = rand.nextInt
}

val booleans = for (x <- integers) yield x > 0

val pairs = for (x <- integers; y <- integers) yield (x, y)

def single[T](x: T): Generator[T] = new Generator[T] {
  def generate: T = x
}

def choose (lo: Int, hi: Int): Generator [Int] =
  for (x <- integers) yield lo + x % (hi - lo)

def oneOf [T] (x : T*): Generator [T] =
  for (idx <- choose (0, x.length)) yield x(idx)

