1 + 1

val x = 42

x * x

val y = 30
  

def sum(xs: List[Int]): Int = 
    if(xs.isEmpty) then 0
    else xs.head + sum(xs.tail)

val test = sum(List(1,2,3,4))

def fact(x: Int): Int =
  def aux(x: Int, acc: Int): Int =
    if x == 0 then acc
    else aux(x - 1, acc * x)

  aux(x, 1)

fact(5)

def sum(f: Int => Int, a: Int, b: Int): Int =
    def loop(a: Int, acc: Int): Int =
      if a > b then acc 
      else loop(a+1, acc + f(a))
    loop(a, 0)

def sumV2(f: Int => Int): (Int, Int) => Int = 
    def sumF(a: Int, b: Int): Int = 
      if a > b then 0 
      else f(a) + sumF(a + 1, b)
    sumF

def sumIntsV1 = sumV2(x => x)
def sumCubesV1 = sumV2(x => x * x * x)
sumIntsV1(2,4)
sumCubesV1(2,4)

def sumV3(f: Int => Int)(a: Int, b: Int): Int = 
  if a > b then 0 else f(a) + sumV3(f)(a + 1, b)

def sumIntsV2 = sumV3(x => x)
def sumCubesV2 = sumV3(x => x * x * x)
sumIntsV2(2,4)
sumCubesV2(2,4)