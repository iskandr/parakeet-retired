dist: { 0 +/ (x-y) * (x-y) }
f:{ x dist/:\: y}
x: { 3 ? 10 } each til 1000000;
y: { 3 ? 10 } each til 10; 
z: f[x;y]
