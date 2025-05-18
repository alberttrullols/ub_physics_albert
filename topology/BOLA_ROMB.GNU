file1="BOLAromb.dat"

set term png
set output "bola_romb.png"
set size ratio 1
set title "d_1 DISTÀNCIA DEL ROMB"
set xlabel "x"
set ylabel "y"
set yrange[-3:3]
set xrange [-3:3]


plot file1  