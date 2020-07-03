./a.out 10Cn pars.txt user_pars_10C.txt
./a.out 30Cn pars.txt user_pars_30C.txt
./a.out 10Cs pars_slow.txt user_pars_10C.txt
./a.out 30Cs pars_slow.txt user_pars_30C.txt
./a.out 10Cf pars_fast.txt user_pars_10C.txt
./a.out 30Cf pars_fast.txt user_pars_30C.txt

R CMD BATCH plot.R
