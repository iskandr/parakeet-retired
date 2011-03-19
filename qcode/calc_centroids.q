cc: { [X;a;i] avg X[where a = i]};
ccs: { [X;a;k] cc[X;a] each til k }
X: { 290 ? 100} each til 100000; 

k:2; 
a: 10000 ? k;
z: ccs[X;a;k];
 
