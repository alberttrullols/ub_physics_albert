file1="BOLAchev.dat"

set term png
set output "bola_chev.png"
set size ratio 1
set title "d_∞ DISTÀNCIA CHEVYSEV"
set xlabel "x"
set ylabel "y"
set yrange[-3:3]
set xrange [-3:3]


plot file1  


