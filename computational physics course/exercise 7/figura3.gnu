set term png
set output "P7-22-23-fig3.png"
set title 'Oscil·lacions Grans'
set xzeroaxis
set yzeroaxis
set xrange [-40:7]
set yrange [-8:8]
set ylabel 
set xlabel 'φ(rad)'
set ylabel 'dφ/dt (rad/s)'
set key top left
plot "P7-22-23-res.dat" index 1 using 2:4 t "Mètode d'Euler", "P7-22-23-res.dat" index 1 using 3:5 t "Mètode d'Adams"