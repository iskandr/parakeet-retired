dist: { sum (x-y)*(x-y) };
calc_centroid:{ [X;a;i] avg X[where a = i] }
calc_centroids:{[X;a;k] calc_centroid[X;a] each til k};

kmeans_body: { [X;C;a;k]
    D: X dist/:\: C;
    a2: { x ? min x } each D; 
    C2: calc_centroids[X;a;k];
    (C2;a2) }

kmeans:{ [X;k] 
  n: count X; 
  assignment: n ? k;
  C: calc_centroids[X;assignment;k]; 
  converged: 0b;
  iter: 0;  
  while[not converged; 
    iter: iter + 1; 
    lastAssignment: assignment; 
    result: kmeans_body[X;C;assignment;k]; 
    C: result[0]; 
    assignment: result[1]; 
    converged: all lastAssignment = assignment];
  (C;iter) }

X: { 20 ? 10} each til 1000; 
result: kmeans[X; 3]; 
c: result[0]; 
iters: result[1];
