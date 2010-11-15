/ f:{[x;a;i] avg x[where a = i] }
f: { [x;a;i] avg x[where a = i] }
x: { 4 ? 10 } each til 10000; 
a: { 1 } each til 10000; 
i: 1; 
z: f[x;a;i]

