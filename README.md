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

Compiled versions for Linux (stm) and Windows (stm.exe) are included in the root directory.
To use the example calls given in this document in Windows, change stm to stm.exe and ./ to .\\.
For examples (Windows and Linux) see the `applications` directory (the latest examples are in `STM/applications/CH4_2021/examples`).

Continuing with some guidance on using the program. . .

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
Fresh slurry is added hourly at a fixed rate, and (optionally) removed on one or two days of the year.
Or, by specifying the slurry level, more complicated or irregular management can be simulated.

## Heat flow
Heat flows included are shown in the crude diagram below, where straight lines represent conduction and convection, and a couple colons represent radiation.

```
                  AIR Sun
                   \   :
		    \__:__
                   |      |--Upper~wall--AIR _________  
  SLURRY~addition--|SLURRY|--Lower~wall--SOIL          
		    ------
                      |
                    Floor
                      |
                    SOIL

```

The horizontal line on the right shows the soil surface.
The temperature of the components in CAPS determine heat transfer rates, while others influence only resistance to heat transfer.
So, for example, heat transfer through the floor is determined by the sum of slurry, floor, and soil resistance, and the temperature difference between the soil (not the floor) and slurry.

The model is effectively zero-dimensional, using a lumped capacitance approach.
Slurry loses (or gains) heat through the floor and walls of the structure, each of which have a single temperature and cross-sectional area at any given time step.
Temperatures are based on very simple predictions of soil temperature at the floor depth or the middle of the buried wall depth.
There is no effect of heat loss or gain on the environment--soil does not warm or cool due to interaction with slurry.
Heat transfer resistance are essentially "overall" values that include multiple modes of heat transfer, with units of K-m2/W.
For the floor and wall components, heat flux within a time step is therefore:

```
Q = (T(slurry) - T(environment)) / R * A
```

where R is the overall resistance term, and A is the cross-sectional area available for heat transfer.
The overall resistance term for any route is simply the sum of relevant terms.
The model uses the following expressions.

```
  R(floor) = R(slurry) + R(wall) + R(soil)
  R(dwall) = R(slurry) + R(wall) + R(soil)
  R(uwall) = R(slurry) + R(wall) + R(air)
  R(top) =   R(slurry) + R(air) 
```

The resistance term for air, like all others for individual components, is calculated externally by the user, and can include the effect of a cover.
Any resistance from the temperature gradient that forms in slurry is included in the slurry resistance term.

Radiation, taken as the product of solar radiation and slurry surface absorptivity, can only increase slurry temperature.
Because the model simulates *average* slurry temperature, and does not explicitly include heat loss by radiation, the absorptivity value is effectively a net value.
Actual absorptivity may be higher--the surface may absorb a greater fraction of the incident radiation.
But some of this heat will be lost to the air and through other routes, to a larger degree than reflected in model results, due to surface warming.
Radiation may also drive evaporation.
This heat energy consumed by the phase change does not go toward warming slurry, and so should not be expected to be included in the absorptivity value.

## Numerical solution
A simple first-order fixed time step approach is used.
The time step is one hour, although weather is taken as constant over each day.
To avoid numerical instability (probably only a problem when slurry mass (depth) is very low), a steady-state temperature is calculated at each time step, and taken as the slurry temperature if rate calculations suggest that the slurry temperature should surpass the steady-state value.

## Freezing
Freezing and thawing of slurry are included, mainly to capture the behavior of slurry around a temperature of 0 degrees C.
However, the same is not true for soil.
The mass of frozen slurry is tracked separately from the total slurry mass.
During periods of cooling, slurry temperature may drop below 0 degrees C only once it has completely frozen.
Conversely, during warming, frozen slurry cannot exceed 0 degrees C--any heat transferred into the slurry is first used for the phase change.

## Weather
Weather may be given in an input file as daily average air temperature and solar radiation by the day of the year.
Or, minimum, maximum, and day of year of the maximum be specified in the `user_par` file, and used internally with a sine function to estimate daily values.
With either approach, weather inputs are daily; the model does not accept nor simulate hourly weather.

## Missing components
The model does not include:
* Explicit heat loss by radiation (but see information on radiative heating above).
* Any effects of precipitation (although specification of slurry level directly and acceptance that absorptivity excludes radiation that goes toward evaporation may address this limitation)
