\l ../Q/parakeet.q
pq_report_error: {[msg] 2[,["PQ ERROR: "; ,[msg; "\\n"]]];
  exit[1]};
  pq_module_template: pq_gen_module_template[(enlist ("f"; ("x"; ::); (); "where[>[x; 40]]"))];
  pq_template0: pq_get_function_template[pq_module_template; "f"];
  [x: ?[50000;100];
  [pq_old_f: {[x] where[>[x; 40]]};
  f: {[pqarg0_x] pqvalue: pq_run_template[pq_template0; (); (pqarg0_x; ::)];
  $[=[pqvalue[0]; 0];pqvalue[1];$[=[pqvalue[0]; 1];pq_old_f[pqarg0_x; ::];pq_report_error[pqvalue[1]]]]}];
  z:f[x]]

/ Perform unit test
\l unit_test.q
unit_result:pq_old_f[x]
unit_output_result["where"; z~unit_result]
\\
