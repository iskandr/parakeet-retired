/ For now, we set these here.  Can change them to get different tolerances.
tol:0.0001

unit_output_result:{[name;rslt]
  (`$":tmp/" , name , "_rslt") 0: enlist string rslt}

unit_float_vec_eq:{[x;y]
  (count x) = sum (abs(x-y)) < tol}

