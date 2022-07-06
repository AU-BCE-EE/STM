# Compile stm.f90 on Linux

gfortran stm.f90 -o stm
cp stm ../tests/stm
mv stm ../bin/stm
