set term png
set output "P6-22-23-fig3.png"
set xzeroaxis
set yzeroaxis
set title "I_3(N=250000) = (34.88±0.15) µm"
set logscale x
set xtics (10000,20000,50000,100000,200000)
set xrange [9000:300000]
set yrange [32:36]
set xlabel "N"
set ylabel "I_3(µm)"
f(x) = 34.88
set key bottom left
plot "P6-22-23-res.dat" index 2 using 1:2:3 with yerrorbars t 'I3', f(x) t 'I_3 (N=250000)'