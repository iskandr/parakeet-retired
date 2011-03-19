/sqr: {x*x};
/ dist: { sqrt  sum sqr x-y };
/ findmin: { x ? min x};

calc_centroid: {[X;a;i] avg X where a = i }; 
calc_centroids: {[X;a;k] calc_centroid[X;a] each til k};
kmeans: { [X;a;k;niters] 
  C: calc_centroids[X;a;k]; 
  do[niters; 
    b: parakeet_minidx[C] each X; 
    C: calc_centroids[X;b;k]];
  C }

X: (100 20 30; 40 50 10; 110 30 33; 44 55 11); 
n: count X;
k: 2;  
a: n ? k;
niters: 4; 
C: kmeans[X;a;k;niters] 
