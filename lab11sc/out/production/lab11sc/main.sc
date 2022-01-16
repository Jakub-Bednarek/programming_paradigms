case class Wektor (vals: Array[Double]) {
  private def +(other: Wektor): Wektor =
  {
    val resized_vectors = resize(vals, other.vals);
    var out_values = Array[Double]();
    for(i <- 0 until resized_vectors._1.length)
      {
        out_values = out_values :+ (resized_vectors._1(i) + resized_vectors._2(i))
      }

      Wektor(out_values)
  }

  def -(other: Wektor): Wektor =
  {
    val resized_vectors = resize(vals, other.vals);
    var out_values = Array[Double]();
    for(i <- 0 until resized_vectors._1.length)
    {
      out_values = out_values :+ (resized_vectors._1(i) - resized_vectors._2(i));
    }

    Wektor(out_values)
  }

  def *+(other: Wektor): Double =
  {
    val resized_vectors = resize(vals, other.vals);
    var out_product = 0.0;

    for(i <- 0 until resized_vectors._1.length)
      {
        out_product = out_product + (resized_vectors._1(i) * resized_vectors._2(i));
      }

      out_product
  }

  def resize(v1: Array[Double], v2: Array[Double]): (Array[Double], Array[Double]) =
  {
    var out1 = Array[Double]();
    var out2 = Array[Double]();

      if(v1.length < v2.length)
      {
        for(i <- 0 until v2.length)
        {
          if(i < v1.length)
            {
              out1 = out1 :+ v1(i);
            }
          else
            {
              out1 = out1 :+ 0.0;
            }
          out2 = out2 :+ v2(i);
        }
      }
      else
        {
          for(i <- 0 until v1.length)
          {
            out1 = out1 :+ v1(i);

            if(i < v2.length)
              {
                out2 = out2 :+ v2(i)
              }
            else
              {
                out2 = out2 :+ 0.0
              }
          }
        }

    (out1, out2)
  }

  def print(): Unit =
  {
    println("Values of vector:\n");
    for(i <- vals)
      {
        println(i)
      }
  }
}

val v1 = Wektor(Array(1.0, 2.0, 3.0))
val v2 = Wektor(Array(1.0))
val v3 = Wektor(Array(100.0, 200.0, 300.0, 400.0))

val sum = v1 + v2
val diff = v1 - v2
val prod = v1 *+ v2

sum.print();
diff.print()

val sum_1 = v1 + v3
val diff_1 = v1 - v3
val prod_1 = v1 *+ v3

val sum_0 = (v1 + v2) *+ v3

sum_1.print()
diff_1.print()