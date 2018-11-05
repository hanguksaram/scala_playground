import scala.annotation.tailrec
//summ of int's range between two
//with tail optimization and carryng (syntax sugar)
def sumInts(f: Int => Int)(a: Int, b: Int) = {
  @tailrec def sumIntsUtill(a: Int, acc: Int): Int = {
    if (a >= b) 0 else if (a == b - 1 ) acc else sumIntsUtill(a + 1, acc + f(a + 1))
  }
  sumIntsUtill(a + 1, f(a + 1))
}
//without tail optimization without carryn
def sumIntsNoTail(f: Int => Int, a: Int, b: Int): Int = {
  if (a >= b) 0 else if (a == b - 1) 0 else f(a + 1) + sumIntsNoTail(f, a + 1,  b)
}
//sum of int's cubes
def sumCubs(a: Int, b: Int) = sumInts(x => x * x * x)(a, b)
//val sumNoTail
//mapReduce implementation
def mapReduceTailRec(Mapper: Int => Int, Reducer: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  @tailrec def mapReduceTailRecUtil(a: Int, acc: Int): Int = {
    if (a > b) acc else mapReduceTailRecUtil(a + 1, Reducer(Mapper(a), acc))
  }
  mapReduceTailRecUtil(a, zero)

}
def mapReduce(Mapper: Int => Int, Reducer: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if (a > b) zero
  else Reducer(Mapper(a), mapReduce(Mapper, Reducer, zero)(a + 1, b))
}
//factorial via mapreduce
def reduceTailRecFactorial(n: Int) = mapReduceTailRec(x => x, (x, y)=> x * y, 1)(1, n)
def mapFactorial(f: Int => Int)(n: Int) = mapReduce(f, (x, y) => x * y, 1)(1, n)
def factorial(n: Int) = mapReduce(x => x, (x, y) => x * y, 1)(1, n)
def doublingFactorial(n: Int) = mapFactorial(x => x * x)(n)
println(reduceTailRecFactorial(4))
println(factorial(4))
println(doublingFactorial(3))
println(sumCubs(1, 3))
println(sumIntsNoTail(x => x ,1 , 4))