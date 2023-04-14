reset
set size 1, 1
set term png size 1200, 800
#set term svg
set title "Recursive fibonacci performance [n=35]"
set xlabel "Programming language"
set ylabel "Duration [ms]"
set output "timings-evaluator-fib.png"
#set output "timings-evaluator-fib.svg"
set style fill solid
set boxwidth 0.5
set xtics rotate by -30
#set logscale y
plot "timings-evaluator-fib.dat" using 2:xtic(1) with boxes notitle linecolor rgb "black", \
     "timings-evaluator-fib.dat" using 0:($2+300):2 with labels notitle
