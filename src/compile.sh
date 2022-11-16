# Compile stm.f90 on Linux

gfortran stm.f90 -o stm
#cp stm ../tests/stm
mv stm ../bin/Linux/stm

echo 'Compiled stm (or tried to). Did you update version number in stm.f90?'
grep -i -n 'STM version' stm.f90 
