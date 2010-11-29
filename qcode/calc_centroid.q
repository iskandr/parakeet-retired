f: { [x;a;i] avg x[where a = i]  }
n: 10000; 
x: { 128 ? 10.0e } each til n; 
a: { 1 } each til n; 
i: 1; 
z: f[x;a;i]

