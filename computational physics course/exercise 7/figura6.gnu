set term png
set output "P7-22-23-fig6.png"
set xzeroaxis
set yzeroaxis
set title 'Estudi Convergència'
set xrange [0:27]
set yrange [12:20]
set ylabel 'E (J)'
set xlabel 't(s)'
plot "P7-22-23-res.dat" index 4 using 1:2 t 'N=25000' , "P7-22-23-res.dat" index 4 using 3:4 t 'N=1250', "P7-22-23-res.dat" index 4 using 5:6 t 'N=650', "P7-22-23-res.dat" index 4 using 7:8 t 'N=400'