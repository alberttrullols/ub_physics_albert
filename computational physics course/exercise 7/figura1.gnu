set term png
set output "P7-22-23-fig1.png"
set xzeroaxis
set yzeroaxis
set title 'Petites Oscil·lacions'
set xrange [0:12]
set yrange [-0.03:0.03]
set xlabel 't(s)'
set ylabel 'φ (rad)'
f(x) = 0.012*cos(3.12*x)
plot "P7-22-23-res.dat" index 0 using 1:2 t "Mètode d'Euler", "P7-22-23-res.dat" index 0 using 1:3 t "Mètode d'Adams", f(x)