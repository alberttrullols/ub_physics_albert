set term png
set output "Cv_T.png"
set xzeroaxis
set yzeroaxis
set xrange [1:4]




set ylabel "cv*"
set xlabel "T"
set yrange [0:2.5]
plot "SIM-L8-MCTOT10000_res.out" using 2:(($1)*($8**(1))/($2**2)) title 'L=8', "SIM-L16-MCTOT10000_res.out" using 2:(($1)*($8**(1))/($2**2)) title 'L=16',"SIM-L24-MCTOT10000_res.out" using 2:(($1)*($8**(1))/($2**2)) title 'L=24', "SIM-L32-MCTOT10000_res.out"  every :: 5 using 2:(($1)*($8**(1))/($2**2)) title 'L=32'