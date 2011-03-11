sqr: {x*x};
dist: { sqrt  sum sqr x-y };
calc_centroid: {[X;a;i] avg X where a = i }; 
calc_centroids: {[X;a;k] calc_centroid[X;a] each til k};
findmin: { x ? min x};
kmeans: { [X;k;niters] 
  n: count X; 
  a: n ? k;
  C: calc_centroids[X;a;k]; 
  do[niters; 
    D: X dist/:\: C;
    a: findmin each D; 
    C: calc_centroids[X;a;k]];
  C }

X: (100 20 30; 40 50 10; 110 30 33; 44 55 11); 
C: kmeans[X; 2; 4] 
