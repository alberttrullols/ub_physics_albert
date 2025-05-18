set term png
set output "magne_T_abs.png"
set xzeroaxis
set yzeroaxis
set xrange [0:5]

L=32

N=L*L


set key above
set ylabel "Magne/N"
set xlabel "T"
set yrange [0:1.2]
plot "SIM-L8-MCTOT10000_res.out" using 2:6 title 'L=8' with lines, "SIM-L16-MCTOT10000_res.out" using 2:6 title 'L=16' with lines,"SIM-L24-MCTOT10000_res.out" using 2:6 title 'L=24' with lines , "SIM-L32-MCTOT10000_res.out" every ::5 using 2:6 title 'L=32' with lines
