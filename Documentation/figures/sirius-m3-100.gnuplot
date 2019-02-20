set terminal svg
set output "sirius-m3-100.svg"
set title '[0,0,4] -> [0,0,2] 100 (steps)'
set xlabel 'l'
set ylabel 'kphi'
plot 'm3-100.dat' u 3:7 w lp t 'm3-100'
