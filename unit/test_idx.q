\l ../Q/parakeet.q
pq_report_error: {[msg] 2[,["PQ ERROR: "; ,[msg; "\\n"]]];
  exit[1]};
  pq_module_template: pq_gen_module_template[(enlist ("f"; ("x"; "y"; ::); (); "x[y]"))];
  pq_template0: pq_get_function_template[pq_module_template; "f"];
  [x: ?[50000; 100];
  c: ?[100; 50000];
  [pq_old_f: {[x; y] x[y]};
  f: {[pqarg1_x; pqarg0_y] pqvalue: pq_run_template[pq_template0; (); (pqarg1_x; pqarg0_y; ::)];
  $[=[pqvalue[0]; 0];pqvalue[1];$[=[pqvalue[0]; 1];pq_old_f[pqarg1_x; pqarg0_y; ::];pq_report_error[pqvalue[1]]]]}];
  z: f[x; c]]

/ Perform unit test
\l unit_test.q
unit_result:pq_old_f[x;c]
unit_output_result["idx"; z~unit_result]
\\
