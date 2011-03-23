cc: { [X;a;i] avg X[where a = i]};
ccs: { [X;a;k] cc[X;a] each til k }
kmeans: { [X;a;k;niters] 
  C: ccs[X;a;k]; 
  do[niters; 
    b: parakeet_minidx[C] each X; 
    C: ccs[X;b;k]];
  C }

n: 32000;
d: 300;
k: 30;  
a: n ? k;
X: { d ? 10e } each til n; 
niters: 50;
C: kmeans[X;a;k;niters]

