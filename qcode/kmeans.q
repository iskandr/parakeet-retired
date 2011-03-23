cc: { [X;a;i] avg X[where a = i]};
ccs: { [X;a;k] cc[X;a] each til k }
kmeans: { [X;a;k;niters] 
  C: ccs[X;a;k]; 
  do[niters; 
    b: parakeet_minidx[C] each X; 
    C: ccs[X;b;k]];
  C }

n: 131072;
d: 30;
k: 3;  
a: n ? k;
X: { d ? 10e } each til n; 
niters: 6;
C: kmeans[X;a;k;niters]

