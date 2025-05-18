set term png
set output "fig1.png"
file = 'P9-22-23-dimarts.dat'

set title 'CONVERGÈNCIA JACOBI'
set xzeroaxis
set yzeroaxis
set xrange [0:20000]
set yrange [0:300]

set ylabel "T_p(ºC)"
set xlabel "iteracions"

plot file index 0  t 'T_0 = 15ºC' , file index 3  t 'T_0 = 220ºC', file index 6 with lines t 'T_0 = 1280ºC'