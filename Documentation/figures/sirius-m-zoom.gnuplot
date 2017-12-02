set terminal svg
set output "sirius-m-zoom.svg"
set title '[0,0,1] -> [0,0,6] s1/s2 (starting point) 10/100 (steps)'
set xlabel 'l'

set ylabel 'kphi m1'
set ytics nomirror

set y2label 'kphi m2'
set y2tics
plot \
     'm1-101.dat' using 3:7 axis x1y1 with lp title 'm1-100',\
     'm2-101.dat' using 3:7 axis x1y2 with lp title 'm2-100'
