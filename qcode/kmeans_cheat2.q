cc: { [X;a;i] avg X[where a = i]};
ccs: { [X;a;k] cc[X;a] each til k }
f: {[X;a;k] C: ccs[X;a;k]; 
            a2: parakeet_minidx[C] each X; C2: ccs[X;a2;k]; 
            a3: parakeet_minidx[C2] each X; C3: ccs[X;a3;k]; 
            a4: parakeet_minidx[C3] each X; ccs[X;a4;k] }
n: 10000;
d: 10;
k: 3;
a: n ? k;
X: { d ? 100.0e } each til n;
z: f[X;a;k]
