dist: { sqrt 0 +/ (x-y) * (x-y) }
f:{ x dist/:\: y}
x: { 100 ? 10.0e } each til 120000;
y: { 100 ? 10.0e } each til 200; 
z: f[x;y]
