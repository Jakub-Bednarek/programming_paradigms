def sum_arrays_rec(matrix: Array[Array[Int]]): Array[Int] =
  def sum_rec(index: Int, length: Int, out_array: Array[Int]) : Array[Int] =
    if index < length then sum_rec(index + 1, length, out_array :+ matrix(index).foldLeft(0)(_ + _))
    else out_array
  sum_rec(0, matrix.length, Array())

def sum_arrays_imp(matrix: Array[Array[Int]]) : Array[Int] =
  var out_arr = Array[Int]()
  for (i <- matrix){
    var sum = 0
    for (j <- i) {
      sum += j
    }
    out_arr = out_arr :+ sum
  }
  out_arr

sum_arrays_rec(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)))
sum_arrays_imp(Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9)))
