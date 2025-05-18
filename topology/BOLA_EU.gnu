file1="BOLAeu.dat"

set term png
set output "bola_eu.png"
set size ratio 1
set title "d_2 DISTÀNCIA EUCLIDIANA"
set xlabel "x"
set ylabel "y"
set yrange[-3:3]
set xrange [-3:3]


plot file1  

