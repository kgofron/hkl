set terminal svg
set output "traj_n.svg"

set title '[0,0,1] -> [0,0,6] 1 -> 100 (steps)'
set xlabel 'n steps'
set ylabel 'range'
plot 'traj_n.dat' using 1:2 with lp title 'kphi range'
