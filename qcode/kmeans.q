/sqr: {x*x};
/ dist: { sqrt  sum sqr x-y };
/ findmin: { x ? min x};

calc_centroid: {[X;a;i] avg X where a = i }; 
calc_centroids: {[X;a;k] calc_centroid[X;a] each til k};
kmeans: { [X;a;k;niters] 
  C: calc_centroids[X;a;k]; 
  do[niters; 
    a: parakeet_minidx[C] each X; 
    C: calc_centroids[X;a;k]];
  C }

n: 10000;
d: 10; 
k: 1;  
a: n ? k;
X: { d ? 100e } each til n; 
niters: 100; 
C: kmeans[X;a;k;niters] 
