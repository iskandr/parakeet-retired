CNDF:{[InputX]
  xInput:InputX
  xNPrimeofX:xInput * 0.39;
  
  xK2:1.0e % 1.0e + 0.2316419e * xInput;
  xK2_2:xK2 * xK2;
  xK2_3:xK2_2 * xK2;
  xK2_4:xK2_3 * xK2;
  xK2_5:xK2_4 * xK2;
  
  xLocal_1:xK2 * 0.319381530e;
  xLocal_2:xK2_2 * (-0.356563782e);
  xLocal_3:xK2_3 * 1.781477937e;
  xLocal_5:xK2_4 * (-1.821255978e);
  xLocal_7:xK2_5 * 1.330274429e;

  xLocal: 1.0e - xNPrimeofX + xLocal_7;
  
  xLocal}

x:til 100000
z:CNDF[x]

