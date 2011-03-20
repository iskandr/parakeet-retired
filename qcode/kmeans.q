cc: { [X;a;i] avg X[where a = i]};
ccs: { [X;a;k] cc[X;a] each til k }
kmeans: { [X;a;k;niters] 
  C: ccs[X;a;k]; 
  do[niters; 
    a: parakeet_minidx[C] each X; 
    C: ccs[X;a;k]];
  C }

n: 100000;
d: 10;
k: 2;  
a: n ? k;
X: { d ? 100e } each til n; 
niters: 2; 
C: kmeans[X;a;k;niters] 
