# STM
Simple heat transfer model for predicting the temperature of animal slurry (animal manure with < about 15% dry matter) during storage in an outdoor tank.
Earlier versions were focused on indoor channels.

# Usage
The model code is in `stm.f90`.
On Linux with the f95 compiler, it can be compiled with the following command.

```
gfortran stm.f90 -o stm
```
If compiled to `stm` (as from above command), then:
stm <IDxx> <par_file_name> <user_par_file_name> <weather_file_name>

IDxx is a 4 character run ID code.
For example:

./stm 10Cn pars.txt user_pars.txt

The model can be called with no files (and default names will be used, with weather calculated), with just the 2 parameter files (and calculated weather), or with all three files. 
See par and weather files in main directory for details on contents.

# Inputs
Set inputs in user_pars.txt. Help on calculating parameter values can be found in the par_calc directory. The pars.txt file has parameters directly related to heat transfer and storage, including heat capacity and convection coefficients. Ideally these values would not be regularly changed, but determination of the values of some is challenging.
  
# Outputs
Files are created for temperature, weather, and heat transfer rates. See application directories for examples.

# Model description
The model predicts the average temperature of slurry within a storage structure.
Fresh slurry is added at a fixed rate, and (optionally) removed on one or two days of the year.

## Heat flow
Heat flows included are shown in the crude diagram below, where straight lines represent conduction and convection, and `}` represents radiation.

```
                  Air Sun
                   |   }
  Slurry addition-SLURRY-Wall-Soil
                     |
                   Floor
                     |
                   Soil
```

The model is effectively zero-dimensional, using a lumped capacitance approach.
Slurry loses (or gains) heat from the floor and walls of the structure, each of which have a single temperature and cross-sectional area at any given time step.
Temperatures are based on very simple predictions of soil temperature at the floor depth or the middle of the buried wall depth.
Heat transfer coefficients are essentially "overall" values that include multiple modes of heat transfer, with units of W/m-K.
The value for slurry to air is calculated externally by the user, and can include the effect of a cover.
Additional resistance from the temperature gradient that forms in slurry is included internally.

```
U = 1 / (1 / U1 + L2 / k2)
```

For the floor and wall, coefficients are calculated by the model from thermal conductivity and an estimate of the distance over which a temperature gradient is present.

```
U = 1 / (L1 / k1 + L2 / k2)
```

The gradient lengths provide a simple way to include the complex effect of temperature gradients on transfer rates.
For slurry, the parameter represents the degree of mixing.

Radiation, taken as the product of solar radiation and slurry surface emissivity, can only increase slurry temperature.

## Numerical solution
A simple first-order fixed time step approach is used.
The time step is one hour, although weather is taken as constant over each day.
To avoid numerical instability (probably only a problem when slurry mass (depth) is very low), a steady-state temperature is calculated at each time step, and taken as the slurry temperature if rate calculations suggest that the slurry temperature should surpass the steady-state value.

## Freezing
Freezing and thawing are included, mainly to capture the behavior of slurry around a temperature of 0 degrees C.

## Weather
Weather may be given in an input file as daily average air temperature and solar radiation by the day of the year.
Or, minimum, maximum, and day of year of the maxima can be specified in the user_par file, and used internally with a sine function to estimate daily values.

## Missing components
The model does not include:
* Heat loss by radiation
* Heat transfer through walls exposed to air
* Any effects of precipitation
