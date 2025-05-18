set term png
set output "P6-22-23-fig1.png"
set xzeroaxis
set yzeroaxis
set title "Nombre de Quarks"
set logscale x
set xtics (100,300,1000,3000,10000,30000)
set xrange [100:100000]
set yrange [0.5:3]
set xlabel "N"
set ylabel "n (quarks)"
f(x) = 2 #resultat esperat i1
g(x) = 1 #resultat esperat i2
plot "P6-22-23-res.dat" index 0 using 1:2:3 with yerrorbars t 'n_u', "P6-22-23-res.dat" index 0 using 1:4:5 with yerrorbars t 'n_d', f(x) t 'n_u teòric', g(x) t 'n_d teòric'