dist: { 0 +/ (x-y) * (x-y) }
f:{ x dist/:\: y}
x: { 20 ? 10 } each til 1000; 
y: { 20 ? 10 } each til 10; 
z: f[x;y]
