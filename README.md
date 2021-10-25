# STM
Simple heat transfer model for predicting slurry (animal manure with < about 15% dry matter) temperature during storage in an outdoor tank.

# Usage
If compiled to stm, then:
stm <IDxx> <par_file_name> <user_par_file_name>

For example:

./stm 10Cn pars.txt user_pars.txt

IDxx is a 4 character run ID code.

# Inputs
Set inputs in user_pars.txt. Help on calculating parameter values can be found in the par_calc directory. The pars.txt file has parameters directly related to heat transfer and storage, including heat capacity and convection coefficients. Ideally these values would not be regularly changed, but determination of the values of some is challenging.
  
# Outputs
Files are created for temperature, weather, and heat transfer rates. See application directories for examples.
