set term png
set output "fig3.png"
file = 'P9-22-23-dimarts.dat'

set title 'CONVERGÈNCIA SOBRERELAXACIÓ'
set xzeroaxis
set yzeroaxis
set xrange [0:3300]
set yrange [0:300]

set ylabel "T_p(ºC)"
set xlabel "iteracions"

plot file index 2  t 'T_0 = 15ºC', file index 5  t 'T_0 = 220ºC', file index 8 with lines t 'T_0 = 1280ºC'