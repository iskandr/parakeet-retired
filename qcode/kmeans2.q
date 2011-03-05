dist: { sum (x-y)*(x-y) };
calc_centroid:{ [X;a;i] avg X[where a = i] }
calc_centroids:{[X;a;k] calc_centroid[X;a] each til k};
minidx: {x ? min x}; 

kmeans:{ [X;k] 
  n: count X; 
  assignment: n ? k;
  C: calc_centroids[X;assignment;k]; 
  converged: 0b;
  iter: 0;  
  while[not converged; 
    iter: iter + 1; 
    lastAssignment: assignment; 
    D: X dist/:\: C;
    a: minidx each D; 
    C: calc_centroids[X;a;k];
    converged: all lastAssignment = assignment];
  C }

X: { 20 ? 10} each til 1000; 
result: kmeans[X; 3]; 
c: result[0]; 
iters: result[1];
