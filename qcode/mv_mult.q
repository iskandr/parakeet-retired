vv_mult: { sum x * y } 
mv_mult: { vv_mult[y] each x }

x: (1 2 3; 4 5 6; 7 8 9);
y: -2 1 2;
z: mv_mult[x;y]
