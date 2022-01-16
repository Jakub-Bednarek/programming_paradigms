// Zadanie 1

def get_pi(precision: Float): Float =
  def get_pi_rec(current_val: Float, current_approx: Float, last_approx: Float): Float =
    if(Math.abs(current_approx - last_approx) < precision) then 2.0.toFloat / current_val
    else
      val new_val = Math.sqrt(0.5 + (0.5 * current_val)).toFloat
      get_pi_rec(new_val * current_val, new_val, current_approx)
  get_pi_rec(Math.sqrt(0.5).toFloat, Math.sqrt(0.5).toFloat, 0)

get_pi(0.1)
get_pi(0.01)
get_pi(0.001)
get_pi(0.0001)

get_pi(0.00001)
get_pi(0.000001)
get_pi(0.0000001)