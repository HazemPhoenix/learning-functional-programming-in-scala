
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


def product(f: Int => Int)(a: Int, b: Int): Int =
    if a > b then 1 else f(a) * product(f)(a + 1, b)

def prodInts = product(x => x)
def prodSqr  = product(x => x * x)
def prodFact = product(fact)
def factV2(n: Int)   = product(x => x)(1, n)

prodInts(2,4)
prodSqr(2,4)
prodFact(2,4)
factV2(5)


class Rational(x: Int, y: Int):
  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a%b)

  def this(x: Int) = this(x, 1)
  val g = gcd(x.abs, y)
  
  def numer = x 
  def denom = y 

  def add(r: Rational): Rational =
    Rational(numer * r.denom + denom * r.numer, denom * r.denom)

  def sub(r: Rational): Rational = 
    Rational(numer * r.denom - denom * r.numer, denom * r.denom)

  def mult(r: Rational): Rational =
    Rational(numer * r.numer, denom * r.denom)

  def neg = Rational(-numer, denom)
  
  override def toString = s"${numer / g}/${denom / g}"
end Rational

val a = Rational(1,2)
val b = Rational(3,2)
val neg = a.neg
val negception = neg.neg
val sumRational = a.add(b)
val multRational = a.mult(b)
val subRational = b.sub(a)

val x = Rational(1,3)
val y = Rational(5,7)
val z = Rational(3,2)
x.sub(y).sub(z)

Rational(2)