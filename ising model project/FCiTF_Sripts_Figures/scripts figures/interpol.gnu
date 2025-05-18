set term png
set output "sus_int_T.png"
set xzeroaxis
set yzeroaxis
set xrange [1:4]




set ylabel "cv*"
set xlabel "T"
set yrange [0:40]
plot "interpol.out" using 1:3 title 'sus_int' with lines ,  "SIM-L32-MCTOT10000_res.out" using 2:(($1)*($7-$6**2)/($2))