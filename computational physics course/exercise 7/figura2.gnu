set term png
set output "P7-22-23-fig2.png"
set xzeroaxis
set yzeroaxis
set title 'Oscil·lacions Grans'
set xrange [0:13]
set yrange [-45:10]
set xlabel 't (s)'
set ylabel 'φ(rad)'
plot "P7-22-23-res.dat" index 1 using 1:2 t "Mètode d'Euler", "P7-22-23-res.dat" index 1 using 1:3 t "Mètode d'Adams"