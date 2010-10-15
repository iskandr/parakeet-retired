/ *********************************
/      HELPER FUNCTIONS 
/ *********************************

pi: 3.141592653589793238

/ extracts a list of diagonal elements
diag: {[x] 
		idx: til (count x); 
	    x ./: (idx ,' idx)
	  }
vecmul: {sum x * y } / inner product 	  
row_mat_mul: { sum x * flip y } 
outer: {x vecmul/:\: y}  / Given two vectors, calculate their outer product matrix 
zeros: { (x;y) # 0.0 } 
eye: { idx: til x; `float $ idx =/:\: idx };  
prod: { 1 */ x }
det: { prod diag x } / calculates determinant for triangular matrices, NOT actually accurate. Needs to be replaced. 
uniform: { [size] size # ((prod size) ? 10000) % 10000} 
norm: { sqrt sum x * x } / vector norm, or the norm of a matrix's columns
fnorm: { sqrt sum raze x * x } / frobenius norm of a matrix
sqr_fnorm: { sum raze x * x } 

// *******************************
//     K-NEAREST NEIGHBORS 
// *******************************

// suitable only for smaller data 
calcDist_small: 
          { diff: x -/:\: y;  // diff[i;j] is difference between x[i] and y[j]  
            norm each' diff
          }

calcDist: {  m: count x; 
             n: count y; 
             dists: zeros[m; n];  
             i: 0; 
             while[i < m;
                   diff: x[i] -/: y; 
                   dists[i]: norm each diff;
                   i+:1; 
                  ];
             dists
          } 

// returns indices of each x's k nearest neighbors in y  
knn: { [x;y;k]
       dists: calcDist[x;y];    // distances from each x to each y
       k #' iasc each dists     // first k indices of sorted rows 
     };

// returns k vectors, one for each centroid, with a 1 in the position for each
// vector which is closest to that centroid
// x - list of input vectors
// y - list of centroid vectors
nn: { [x;y]
      dists: calcDist[x;y];
      flip dists =' min each dists
    };

// Weighted k-nearest neighbor regression 
// Inputs:
//   Xnew - data points for which we don't have label s
//   X - data points which are labeled
//   Y - labels for rows of X
//   k - number of neighbors to consider in regression 
 wknn:  { [Xnew; X; Y; k]
            dists: calcDist[Xnew;X]; 
            indices: k #' iasc each dists;
            weights: reciprocal dists @' indices; 
            neighbor_labels: Y @/: indices; 
            weights wavg' neighbor_labels
        };
        

// ********************************
//   K-MEANS CLUSTERING
// ********************************
                    
// Inputs: 
//    X - m*n matrix of data samples
//    k - number of clusters
//    tol - minimum average change in cluster centers per iteration 
//    maxiters - maximum number of iterations

kmeans: {[X;k;tol;maxiters]
			m: count X; 
			n: count X[0];
            idx: k # (neg m) ? m; // take first k indices of an m-element permutation 
            M: X[idx];            // initialize cluster centers to k random data points
            nvec: `boolean $ zeros[k;m]; 
            oldM: M; 
            change: 10e30;      // how much did the average cluster center change per iteration? 
            iter: 0; 
            while[(change > tol) & (iter < maxiters);
                    oldM: M;
                    nvec: nn[X;M];                    
                    indices: where each nvec;
                    M: avg each X @/: indices;   
                    change: (sqr_fnorm M - oldM) % (k * n);
                    1 "[kmeans] iter: " , (string iter) , ", change: " , (string change) , "\n"; 
                    iter+:1; 
                 ];
            (M; nvec)
        };	


// ************************************ 
//        MIXTURE OF GAUSSIANS
// ************************************

// calculate probablity of each row of X under gaussian with mean m and covariance S
mvnpdf: { [X; m; S]
			n: count m; 
			d: reciprocal ((2*pi) xexp n % 2) * (sqrt det S);
			Xm: X -\: m; 
			fn: {[Si; xm] vecmul[row_mat_mul[xm; Si]; xm] };
            d * exp -0.5 * fn[inv `float $ S] each Xm
		}
		
// Inputs:
//	X - m*n data matrix
//	M - k*n  matrix where M[i] is the ith mean vector
//	S - k*n*n matrix where S[i] is the ith covariance matrix
//	mixture - k mixture coefficients
//  Output: k * m matrix of probabilities of data points under each gaussian
calcProbs: {[X;M;S;mixture]
				params: M {(x;y)}' S; 				// form pairs of mean vectors and covariance matrices
				
				mixture  * mvnpdf[X] .' params      // apply mvnpdf[X] to per-cluster pairs of mean/cov,
                                                    // then rescale by mixture coefficients of each gaussian. 
			}

calcNll: {[X;M;S;mixture]
				probs: calcProbs[X;M;S;mixture]; 	// k * m matrix of probabilities under each gaussian
               
                /1 raze (raze (string probs));
                /1 raze (string min each probs) ,\: "\n"; 
                neg sum log sum probs			    // nll = -sum_i(log(sum_k(p(x_{i,k}))))
		 }		
		
// --- calcCov ---   
// Inputs:
//   X - m*n data matrix 
//   M - k*n matrix whose rows are means
//   r - k*m responsibilities 
//   R - reciprocal of columnwise sum of r
// Output: a list of k n*n covariance matrices 
calcCov:{[X;M;r]
			k: count M; 
            n: count M[0]; 
            S: {zeros[n;n]} @' (til k); 
            i:0; 
            while[i<k; 
                Xm: X -\: M[i]; 
                outers: Xm *\:/:' Xm;           // m * n *n list of partial covariance matrices
                S[i]: r[i] wavg outers;
                i+:1; 
                ];
            S
		}

em:{[X;k;tol;maxiters] 
		m: count X;
		temp: kmeans[X;k;tol;maxiters];
		M:temp[0];		 
		r:temp[1];		   // k*m responsibility matrix 
		R: sum flip r;     // total weight per cluster
		mixture: R % m;    // mixture coefficients per gaussian
		S: calcCov[X;M;r];
		
		nll: 1e30;
		prev_nll: 2*nll;
		iter: 0;  
		while[((prev_nll - nll) > tol) & (iter < maxiters);
				/ Expectation Step 
				probs: calcProbs[X;M;S;mixture];			
				r: probs %\:  sum probs; 		// divide weight of X[i] by same normalizer[i] under each cluster
                R: sum flip r;    				// total weight per cluster
                
				/ Maximization Step 
				M: (X wsum/: r) % R;            // weight data by responsibility under each gaussian
                S:calcCov[X;M;r];
				mixture: R % m; 		 	     // mixture coefficients per gaussian	
				
				prev_nll: nll; 
				nll: calcNll[X;M;S;mixture];
				iter +: 1; 
                1 "[em] iter: " , (string iter) , ", nll: " , (string nll) , "\n"; 
			  ]
		(M;S;mixture)
	}

	
/ TEST DATA
n: 512;
m: 16000;
k: 20; 
 
X: uniform (m;n)  
M: uniform[(k; n)] +' (k ? k) % k; 
Y: (neg k) ? k;  
  
/S: {x * eye[n] } each (til k) + 1  
/mixture: uniform k
/mixture %: sum mixture
