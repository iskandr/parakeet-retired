\l ../Q/parakeet.q
pq_report_error: {[msg] 2[,["PQ ERROR: "; ,[msg; "\\n"]]];
  exit[1]};
  pq_module_template: pq_gen_module_template[(("CNDF"; ("InputX"; ::); (); "xInput: ?[<[InputX; 0.e]; neg[InputX]; InputX];  expValues: exp[*[-0.5e; *[xInput; xInput]]];  inv_sqrt_2xPI: 0.398942280401e;  xNPrimeofX: *[expValues; inv_sqrt_2xPI];  xK2: %[1.e; +[1.e; *[0.2316419e; xInput]]];  xK2_2: *[xK2; xK2];  xK2_3: *[xK2_2; xK2];  xK2_4: *[xK2_3; xK2];  xK2_5: *[xK2_4; xK2];  xLocal_1: *[xK2; 0.31938153e];  xLocal_2: *[xK2_2; -0.356563782e];  xLocal_3: *[xK2_3; 1.781477937e];  xLocal_5: *[xK2_4; -1.821255978e];  xLocal_7: *[xK2_5; 1.330274429e];  xLocal: -[1.e; *[xNPrimeofX; +[xLocal_1; +[xLocal_2; +[xLocal_3; +[xLocal_5; xLocal_7]]]]]];  OutputX: ?[<[InputX; 0.e]; xLocal; -[1.e; xLocal]];  OutputX"); ("BlkSchlsEqEuroNoDiv"; ("xStockPrice"; "xStrikePrice"; "xRiskFreeRate"; "xVolatility"; "xTime"; "xCallput"; ::); (); "xLogTerm: log[%[xStockPrice; xStrikePrice]];  xPowerTerm: *[xVolatility; *[xVolatility; 0.5e]];  xDen: *[xVolatility; sqrt[xTime]];  xD1: %[+[xLogTerm; *[xTime; +[xRiskFreeRate; xPowerTerm]]]; xDen];  xD2: -[xD1; xDen];  NofXd1: CNDF[xD1];  NofXd2: CNDF[xD2];  FutureValueX: *[xStrikePrice; exp[neg[*[xRiskFreeRate; xTime]]]];  OptionPrice: ?[=[xCallput; \"C\"]; -[*[xStockPrice; NofXd1]; *[FutureValueX; NofXd2]]; -[*[FutureValueX; -[1.e; NofXd2]]; *[xStockPrice; -[1.e; NofXd1]]]];  OptionPrice"))];
  pq_template1: pq_get_function_template[pq_module_template; "BlkSchlsEqEuroNoDiv"];
  pq_template0: pq_get_function_template[pq_module_template; "CNDF"];
  [ReadOptionFile: {[name] 0:[("EEEEEECEE"; enlist[" "]); $[`; ,[":"; name]]]};
  [pq_old_CNDF: {[InputX] xInput: ?[<[InputX; 0.e]; neg[InputX]; InputX];
  expValues: exp[*[-0.5e; *[xInput; xInput]]];
  inv_sqrt_2xPI: 0.398942280401e;
  xNPrimeofX: *[expValues; inv_sqrt_2xPI];
  xK2: %[1.e; +[1.e; *[0.2316419e; xInput]]];
  xK2_2: *[xK2; xK2];
  xK2_3: *[xK2_2; xK2];
  xK2_4: *[xK2_3; xK2];
  xK2_5: *[xK2_4; xK2];
  xLocal_1: *[xK2; 0.31938153e];
  xLocal_2: *[xK2_2; -0.356563782e];
  xLocal_3: *[xK2_3; 1.781477937e];
  xLocal_5: *[xK2_4; -1.821255978e];
  xLocal_7: *[xK2_5; 1.330274429e];
  xLocal: -[1.e; *[xNPrimeofX; +[xLocal_1; +[xLocal_2; +[xLocal_3; +[xLocal_5; xLocal_7]]]]]];
  OutputX: ?[<[InputX; 0.e]; xLocal; -[1.e; xLocal]];
  OutputX};
  CNDF: {[pqarg0_InputX] pqvalue: pq_run_template[pq_template0; (pqarg0_InputX; ::); ()];
  $[=[pqvalue[0]; 0];pqvalue[1];$[=[pqvalue[0]; 1];pq_old_CNDF[pqarg0_InputX; ::];pq_report_error[pqvalue[1]]]]}];
  [pq_old_BlkSchlsEqEuroNoDiv: {[xStockPrice; xStrikePrice; xRiskFreeRate; xVolatility; xTime; xCallput] xLogTerm: log[%[xStockPrice; xStrikePrice]];
  xPowerTerm: *[xVolatility; *[xVolatility; 0.5e]];
  xDen: *[xVolatility; sqrt[xTime]];
  xD1: %[+[xLogTerm; *[xTime; +[xRiskFreeRate; xPowerTerm]]]; xDen];
  xD2: -[xD1; xDen];
  NofXd1: pq_old_CNDF[xD1];
  NofXd2: pq_old_CNDF[xD2];
  FutureValueX: *[xStrikePrice; exp[neg[*[xRiskFreeRate; xTime]]]];
  OptionPrice: ?[=[xCallput; "C"]; -[*[xStockPrice; NofXd1]; *[FutureValueX; NofXd2]]; -[*[FutureValueX; -[1.e; NofXd2]]; *[xStockPrice; -[1.e; NofXd1]]]];
  OptionPrice};
  BlkSchlsEqEuroNoDiv: {[pqarg0_xStockPrice; pqarg1_xStrikePrice; pqarg2_xRiskFreeRate; pqarg3_xVolatility; pqarg4_xTime; pqarg5_xCallput] pqvalue: pq_run_template[pq_template1; (pqarg0_xStockPrice; pqarg1_xStrikePrice; pqarg2_xRiskFreeRate; pqarg3_xVolatility; pqarg4_xTime; pqarg5_xCallput; ::); ()];
  $[=[pqvalue[0]; 0];pqvalue[1];$[=[pqvalue[0]; 1];pq_old_BlkSchlsEqEuroNoDiv[pqarg0_xStockPrice; pqarg1_xStrikePrice; pqarg2_xRiskFreeRate; pqarg3_xVolatility; pqarg4_xTime; pqarg5_xCallput; ::];pq_report_error[pqvalue[1]]]]}];
  options: ReadOptionFile["../unit/bs_2M.txt"];
  xStockPrice: options[`Stockprice];
  xStrikePrice: options[`Strikeprice];
  xRiskFreeRate: options[`Rate];
  xVolatility: options[`Volatility];
  xTime: options[`Time];
  xCallput: options[`Callput];
  result: BlkSchlsEqEuroNoDiv[xStockPrice; xStrikePrice; xRiskFreeRate; xVolatility; xTime; xCallput]];

/ Perform unit test
\l unit_test.q
unit_result:pq_old_BlkSchlsEqEuroNoDiv[xStockPrice; xStrikePrice; xRiskFreeRate; xVolatility; xTime; xCallput]
unit_output_result["bs"; unit_float_vec_eq[result;unit_result]]
\\
