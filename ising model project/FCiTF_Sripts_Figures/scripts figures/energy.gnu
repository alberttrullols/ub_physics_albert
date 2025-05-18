set term png
set output "energia_T.png"
set xzeroaxis
set yzeroaxis
set xrange [1:4]



set key above
set ylabel "Energy/N"
set xlabel "T*"
set yrange [-2.5:0]
plot "SIM-L8-MCTOT10000_res.out" using 2:3 with lines title 'L=8', "SIM-L16-MCTOT10000_res.out" using 2:3 with lines title 'L=16', "SIM-L24-MCTOT10000_res.out" using 2:3 with lines title 'L=24', "SIM-L32-MCTOT10000_res.out" using 2:3with lines title 'L=32'

