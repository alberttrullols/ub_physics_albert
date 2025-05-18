set term png
set output "P7-22-23-fig5.png"
set xzeroaxis
set yzeroaxis
set title 'Estudi transició'
set xrange [-10:65]
set yrange [-10:10]
set xlabel 'φ(rad)'
set ylabel 'dφ/dt (rad/s)'
plot "P7-22-23-res.dat" index 3 using 1:2 t 'dφ/dt = 2sqrt (g/l) - 0.05 rad/s ', "P7-22-23-res.dat" index 3 using 3:4 t 'dφ/dt = 2sqrt (g/l) + 0.05 rad/s '