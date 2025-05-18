set term png
set output "P6-22-23-fig2.png"
set xzeroaxis
set yzeroaxis
set title "I_2(N=1000000) = 0.4997±0.0003"
set logscale x
set xtics (10000,30000,100000,300000,1000000)
set xrange [9000:1100000]
set yrange [0.49:0.51]
set xlabel "N"
set ylabel "I_2 "
f(x) = 0.5
set key bottom left
plot "P6-22-23-res.dat" index 1 using 1:2:3 with yerrorbars t 'I_2', f(x) t 'Iteorica'