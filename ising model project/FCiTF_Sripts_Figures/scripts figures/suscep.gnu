set term png
set output "susceptibilitat.png"
set xzeroaxis
set yzeroaxis
set xrange [0:5]

L=32

N=L*L

set ylabel "χ*"
set xlabel "T"
set yrange [0:30]
plot "SIM-L48-MCTOT20000 _res.out" using 2:(N*($7-$6**2)/($2)) 