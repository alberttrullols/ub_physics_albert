set term png
set output "P7-22-23-fig4.png"
set xzeroaxis
set yzeroaxis
set title "Estudi d'Energies"
set xrange [0:13]
set yrange [0:35]
set ylabel 'E i K (J)'
set xlabel 't(s)'
plot "P7-22-23-res.dat" index 2 using 1:2 t 'K', "P7-22-23-res.dat" index 2 using 1:3 t 'E', "P7-22-23-res.dat" index 2 using 1:4 t 'Ka', "P7-22-23-res.dat" index 2 using 1:5 t 'Ea'