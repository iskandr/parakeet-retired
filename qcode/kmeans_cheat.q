f:{[X;a;k] kmeans[X;a;k]}
d: 10; 
n: 10000; 
k: 3; 
a: n ? k;
X: { d ? 100.0e } each til n; 
C: f[X;k;a]

