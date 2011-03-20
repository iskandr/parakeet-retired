f:{[X;a;k] parakeet_kmeans[X;a;k]}
d: 10; 
n: 12048; 
k: 3; 
a: n ? k;
X: { d ? 100.0e } each til n; 
C: f[X;a;k]

