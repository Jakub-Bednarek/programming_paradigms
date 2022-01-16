//Jakub Bednarek

//Zadanie 2
def curry3[A, B, C, D](f: (A, B, C) => D) (x: A)(y: B)(z: C) = f(x, y, z)
def curry3[A, B, C, D](f: (A, B, C) => D): A => B => C => D = x => y => z => f(x, y, z)

def uncurry3[A, B, C, D](f: A => B => C => D)(x: A, y: B, z: C) = f(x)(y)(z)
def uncurry3[A, B, C, D](f: A => B => C => D): (A, B, C) => D = (x, y, z) => f(x)(y)(z)

//Zadanie 3
def sumProd (lista: List[Int]): (Int, Int) =
  (lista.foldLeft((0, 1)))((sumprod, elem) => (sumprod._1 + elem, sumprod._2 * elem))

sumProd(List(1,2,3,4,5))