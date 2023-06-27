# Compile stm.f90 on Linux

gfortran stm.f90 -o stm
#mv stm ../bin/Linux/stm
mv stm ../../STM-applications/examples/05-2D/stm

echo 'Compiled stm (or tried to). Did you update version number in stm.f90?'
grep -i -n 'STM version' stm.f90 

cd ../../STM-applications/examples/05-2D

./stm large_def pars/pars.txt pars/user_pars_large.txt weather/weather.csv
