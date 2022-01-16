// Jakub Bednarek

import scala.language.postfixOps

//Zadanie 1

def whileLoop(warunek: => Boolean)(wyrazenie: => Unit): Unit =
  if (warunek) then
      wyrazenie
      whileLoop(warunek)(wyrazenie)

var i = 0
whileLoop(i < 3) {
  println(i)
  i = i + 1
}

def swap(tab: Array[Int], i: Int, j: Int) =
  var aux = tab(i); tab(i) = tab(j); tab(j) = aux

val t1 = Array(13, 15, 25)

swap(t1, 0, 1)
t1 == Array(15, 13, 25)

swap(t1, 0, 2)
t1 == Array(25, 13, 15)

def choose_pivot(tab: Array[Int], m: Int, n: Int): Int = tab((m + n) / 2)

def partition(tab: Array[Int], l: Int, r: Int): (Int, Int) =
  var i = l; var j = r; var pivot = choose_pivot(tab, l, r)
  while (i <= j) {
    while (tab(i) < pivot) { i = i + 1 }
    while (pivot < tab(j)) { j = j - 1 }
    if (i <= j) swap(tab, i, j); i = i + 1 ; j = j - 1
  }
  (i, j)

def quick(tab: Array[Int], l: Int, r: Int): Unit =
  if l < r then
    var (i, j) = partition(tab, l, r)
    if j - l < r - i then quick(tab, i, r)
    else quick(tab, l, j)

def quicksort(tab: Array[Int]): Unit = quick(tab, 0, tab.length - 1)

val arr1 = Array(4, 8, 1, 12, 7, 3, 1, 9)
quicksort(arr1)
arr1.toList == List(1, 1, 3, 4, 7, 8, 9, 12)