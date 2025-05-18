set term png
set output "fig5.png"
file = 'P9-22-23-dimarts.dat'
set xzeroaxis
set yzeroaxis
set title 'T(x,y)'
set xrange [0:44.5]
set yrange [0:32.5]
set zrange [0:50]
set xzeroaxis
set yzeroaxis

set xtics out nomirror
set ytics out nomirror

set xlabel "x(cm)"
set ylabel "y(cm)"
set zlabel "T(ºC)"



splot file index 10 u 1:2:3 w pm3d t ''