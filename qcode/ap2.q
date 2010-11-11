dist: { 0 +/ (x-y) * (x-y) }
f:{ x dist/:\: y}
x: { 200 ? 10 } each til 10000; 
y: { 200 ? 10 } each til 10; 
z: f[x;y]
