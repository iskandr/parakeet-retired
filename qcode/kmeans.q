sqr: {x*x};
dist: { sqrt  0+/ sqr x-y };
calc_centroids:{[X;a;k] {[a;i] (0 +/ X where a = i) % sum a = i }[a] each til k};
kmeans:{ [X;k;niters] 
  n: count X; 
  a: n ? k;
  C: calc_centroids[X;a;k]; 
  do[niters; 
    D: X dist/:\: C;
    a: { x ? min x } each D; 
    C: calc_centroids[X;a;k]];
  C }
