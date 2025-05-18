set term png
set output "fig2.png"
file = 'P9-22-23-dimarts.dat'

set title 'CONVERGÈNCIA GAUSS-SEIDEL'
set xzeroaxis
set yzeroaxis
set xrange [0:11000]
set yrange [0:300]

set ylabel "T_p(ºC)"
set xlabel "iteracions"

plot file index 1  t 'T_0 = 15ºC' , file index 4  t 'T_0 = 220ºC', file index 7 with lines t 'T_0 = 1280ºC'