set terminal svg
set output "sirius-m.svg"
set title '[0,0,1] -> [0,0,6] s1/s2 (starting point) 10/100 (steps)'
set xlabel 'l'
set ylabel 'kphi'
plot \
     'm1-11.dat' u 3:7 w lp t 'm1-10',\
     'm2-11.dat' u 3:7 w lp t 'm2-10',\
     'm1-101.dat' u 3:7 w lp t 'm1-100',\
     'm2-101.dat' u 3:7 w lp t 'm2-100'
