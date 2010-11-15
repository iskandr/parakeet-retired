inv_sqrt_2xPI:0.39894228040143270286e

CNDF:{[InputX]
  xInput:?[InputX < 0.0e;neg InputX; InputX];

  expValues:exp -0.5e * xInput * xInput;
  xNPrimeofX:expValues * 0.4e;

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

  xLocal:1.0e - xNPrimeofX * xLocal_1 + xLocal_2 + xLocal_3 + xLocal_5 + xLocal_7;

  OutputX:?[InputX < 0.0e;xLocal;1.0e - xLocal];

  OutputX}

BlkSchlsEqEuroNoDiv:{[xStockPrice; xStrikePrice; xRiskFreeRate; xVolatility; xTime; xCallput]

  xLogTerm: log xStockPrice % xStrikePrice;

  xPowerTerm: xVolatility * xVolatility * 0.5e;

  xDen:xVolatility * sqrt xTime;

  xD1: (xLogTerm + xTime * xRiskFreeRate + xPowerTerm) % xDen;

  xD2: xD1 - xDen;

  NofXd1: CNDF xD1;
  NofXd2: CNDF xD2;

  FutureValueX: xStrikePrice * exp neg xRiskFreeRate * xTime;

  FutureValueX}

x:til 100000
z:CNDF[x]
