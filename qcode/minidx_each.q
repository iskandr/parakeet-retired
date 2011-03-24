f: { [C;X] parakeet_minidx[C] each X };
sqr_dist: { sum x * y }; 
g: { [C;x] minDist:100000000000000.0e; minIdx:0; k: count C; i:0; n: count C[0]; 
       while[i<k; c: C[i]; d: sum c * x; if[d < minDist; minDist:d; minIdx:i]; i: i + 1]; minIdx}; 
h: { [C;X] g[C] each X }; 
   
d: 100; 
n: 100000; 
k: 10; 
X: { d ? 100.0e } each til n; 
C: { d ? 100.0e } each til k; 
a: f[C;X]  

