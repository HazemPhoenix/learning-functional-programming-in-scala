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