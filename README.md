# STM
Simple heat transfer model for predicting the temperature of animal slurry (animal manure with < about 15% dry matter) during storage in an outdoor tank.
Earlier versions were focused on indoor channels.

# Usage
The model code is in `stm.f90`.
On Linux with the GNU Fortran 95 compiler, it can be compiled with the following command.

```
gfortran stm.f90 -o stm
```
If compiled to `stm` (as from above command), then it can be run with this command, for example:

```
stm <IDxx> <par_file_name> <user_par_file_name>
```

`IDxx` is a 4 character run ID code.
For example, an actual call might lok like this:

```
./stm 10Cn pars.txt user_pars.txt
```

The model can be called with no files (and default names will be used, with weather calculated), with just the 2 parameter files (as in all the example above), or with two additional optional files.
With only the two parameter files (as in above example), weather and slurry level variables are calculated from the settings in the user parameter file.
To use measured weather instead, add a weather file to the call:

```
stm <IDxx> <par_file_name> <user_par_file_name> <weather_file_name>
```

The weather file should contain at least these three columns, in this order: day of year, temperature, and average global solar radiation (W/m2).
The file is assumed to have a header row (ignored by the program) and can be whitespace- or comma-delimited.

Slurry level can be set in an additional file, containing at least two columns: day of year, and slurry level (depth) in m.
The only required day of year is 1 (January 1), which is assumed to be the value on the last day of the year also.
The model will use linear interpolation to fill in other days.
Any increase is taken as slurry addition, and decrease removal.

```
stm <IDxx> <par_file_name> <user_par_file_name> <weather_file_name> <slurry_level_file_name>
```

As with the weather file, slurry level can be in a whitespace- or comma-delimited file.

# Inputs
Set inputs in user_pars.txt. 
Help on calculating parameter values can be found in the par_calc directory. 
The pars.txt file has parameters directly related to heat transfer and storage, including heat capacity and heat transfer coefficients. 
Ideally these values would not be regularly changed, but determination of some is challenging.
  
# Outputs
Files are created for temperature, weather, and heat transfer rates. 
See the directories in `applications` for examples.

# Model description
The model predicts the average temperature of slurry within a storage structure.
Fresh slurry is added at a fixed rate, and (optionally) removed on one or two days of the year.
Or, by specifying the slurry level, more complicated or irregular management can be simulated.

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
