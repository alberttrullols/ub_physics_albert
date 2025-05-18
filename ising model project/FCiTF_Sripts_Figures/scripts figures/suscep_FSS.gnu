set term png
set output "susceptibilitat_finite_size.png"
set xzeroaxis

set xrange [-15:20]

gam= 1.76
Tc= 2.28

set ylabel "χscaled"
set xlabel "Tscaled"
set yrange [0:0.05]
plot "SIM-L8-MCTOT10000_res.out" using (($1**0.5)*(($2-Tc)/Tc)):((($1)*($7-$6**2)/($2))/(($1**0.5)**gam)) title 'L=8', "SIM-L16-MCTOT10000_res.out" using (($1**0.5)*(($2-Tc)/Tc)):((($1)*($7-$6**2)/($2))/(($1**0.5)**gam)) title 'L=16', "SIM-L24-MCTOT10000_res.out" using (($1**0.5)*(($2-Tc)/Tc)):((($1)*($7-$6**2)/($2))/(($1**0.5)**gam)) title 'L=24',"SIM-L32-MCTOT10000_res.out" every :: 3 using (($1**0.5)*(($2-Tc)/Tc)):((($1)*($7-$6**2)/($2))/(($1**0.5)**gam)) title 'L=32' 