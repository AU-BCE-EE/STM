# STM
Simple heat transfer model for predicting the temperature of animal slurry (animal manure with < about 15% dry matter) or similar materials during storage in outdoor tanks.
The STM repository has Fortran 90 source code (in `src`), compiled binary files for Linux, macOS, and Windows (in `bin`), and example input files and default parameter file (in `inputs`).
This README file describes the model and gives a short introduction to the software program.
For detailed examples and more information on the software program, see the [STM-applications repo](https://github.com/sashahafner/STM-applications).

# Maintainer and contributors
Sasha D. Hafner developed and maintains this repo.
Find contact information for questions etc. here: <https://au.dk/sasha.hafner@bce>.
Or, open an [issue](https://github.com/AU-BCE-EE/STM/issues).
Data kindly shared by Kristina Mjöfors, Søren O. Petersen, Anna Holm Støckler, and Chih-Yu Hung were used for testing and development.

# Directory structure
* `bin`: binary files (executables) that can be downloaded and used directly without compilation for Windows, macOS, and Linux (but if these don't work, see information on installation and then compilation below)
* `inputs`: minimal inputs as two input files: `pars.txt` contains the latest default parameter set and `user_pars.txt` contains *example* inputs from example 2 in the [STM-applications rep](https://github.com/sashahafner/STM-applications). 
See that repo to check that you have the latest default parameter set, for other examples of `user_par.txt` files, and more information on using the STM program. 

# Installation
Binary files are provided for Linux, macOS, and Windows (32 and 64 bit) in the `bin` directory.
(Compilation on other operating systems is of course possible--see the "Compilation" section below).
In general, "installation" is as simple as downloading the binary file (stm.exe for Windows, stm for Linux) and moving it to the directory from which you would like to call the program.
To run [example 01](https://github.com/sashahafner/STM-applications/tree/master/examples/01) from [STM-applications repo](https://github.com/sashahafner/STM-applications) for example, put stm.exe in the "01" subdirectory and follow the instructions in the [README file](https://github.com/sashahafner/STM-applications/blob/master/examples/01/README.md).
By setting the PATH variable, it is possible to run STM from any directory, although this is not necessary.

The simplest way for non-GitHub users to do download these files is to click on the green "Code" button toward the top right of the repo main page and then "Download ZIP".
Extract the downloaded file, and then browse to the appropriate subdirectory within `bin` to find the binary file.

On Windows, users will likely need to install some libraries to run STM; download and run the [`bin/Windows/w_ifort_runtime. . .exe`](https://github.com/sashahafner/STM/tree/master/bin/Windows) exe file to install them.
And a computer restart is then necessary.
Or, see [here](https://www.intel.com/content/www/us/en/developer/articles/tool/compilers-redistributable-libraries-by-version.html) for the latest versions.

Linux is simpler, but users will need to set file permissions to executable.
Alternatively, users can compile the program themselves (see "Compliation" section below).

To summarize, follow these steps to install STM:

## Windows
1. Download the contents of the STM repo using the green "Code" button and then "Download ZIP"
2. Extract the contents 
3. Find `stm.exe` in `bin` and move it to the appropriate location (e.g., `examples/01` from the [STM-applications repo](https://github.com/sashahafner/STM-applications))
4. Browse to `bin/Windows/w_ifort_runtime. . .exe` and double-click then accept the default settings to install necessary libraries
5. Restart your computer

## Linux and macOS
1. Download [`bin/Linux/stm`](https://github.com/sashahafner/STM/raw/master/bin/Linux/stm) and move it to the appropriate location
2. Set file permissions to executable (e.g., `sudo chmod +x stm`)

# Compilation
The model code is in `src/stm.f90`.
On Linux or macOS with the GNU Fortran 95 compiler, it can be compiled with the following command to create the the binary file `stm`.

```
gfortran stm.f90 -o stm
```

Information on the free GNU compiler is available online.
It can be installed on Linux with `sudo apt install gfortran`.

# Running STM
For several detailed examples and some explanation, see the [STM-applications repo](https://github.com/sashahafner/STM-applications).
A general introduction to the STM program is given here.

If compiled to `stm` on Linux, STM can be run with this command:

```
./stm <ID> <par_file_name> <user_par_file_name> <weather_file_name> <slurry_level_file_name>
```

On Windows, replace `./stm` with `stm.exe`.

`<ID>` is a run ID or key code (1-10 characters).
Providing file names are optional, although the order is fixed.
For example, an actual call might look like this:

```
./stm sim1
```

With no files specified STM will look for the two parameter files with default names (`pars.txt` and `user_pars.txt`), with weather and slurry level calculated.
Typically, however, input file names will be given in the call, as in the example below. 

```
./stm sim2 pars.txt user_pars.txt
```

With only the two parameter files (as in both examples above), weather and slurry level variables are calculated from the settings in the user parameter file.
To use measured weather instead, add a weather file to the call:

```
./stm sim3 pars.txt user_pars.txt weather.csv
```

The weather file should contain these three columns, in this order: day of year (1-365), air temperature (degrees C), and average global solar radiation (W/m2).
The file is assumed to have a header row (ignored by STM) and can be whitespace- or comma-delimited.

Slurry level can be specified in an additional file, containing two columns: day of year and slurry level (depth, m).
The only required day of year is 1 (January 1), and the level is assumed to be the same on the last day of the year also.
The program will use linear interpolation to fill in other days.
Any increase is taken as slurry addition, and decrease removal.

```
./stm sim4 pars.txt user_pars.txt weather.csv slurry_level.csv
```

As with the weather file, slurry level can be in a whitespace- or comma-delimited file.

# Inputs
Example input files can be found in the `inputs` directory.
Inputs in the two "parameter" files have a fixed structure by row, so don't delete or add rows.
The main parameter file, `pars.txt` in the examples above, has parameters directly related to heat transfer and storage, including heat capacity and heat transfer coefficients. 
Ideally these values would not be regularly changed, but determination of some is challenging.
See the [STM-applications repo]((https://github.com/sashahafner/STM-applications) for default values and information on how they were determined.
  
# Outputs
Comma-separated output files are created for temperature, weather, and heat transfer rates. 
See the `tests` directory for examples.

# Bugs and more
If you find a problem in STM or this repo please create an [issue](https://github.com/sashahafner/STM-applications/issues).
If you are familiar with Fortran, please feel free to make some edits and create a pull request!

# Model description
The model predicts the average temperature of slurry within a storage structure.
Fresh slurry is added hourly at a fixed rate, and (optionally) removed on one or two days of the year.
Or, by specifying the slurry level, more complicated or irregular management can be simulated.

## Heat flow
Heat flows included are shown in the ASCII diagram below, where straight lines represent conduction and convection, and asterisks represent radiation.

```
                          Sun
                    AIR    *   
                      \    *    
(SLURRY~addition)----. \   * .----SLURRY~removal
                  || |  \  * | ||
                  ||^^^^^^^^^^^||Upper~wall--AIR
                  ||           ||~~~~~~~~~~~~~~~~~~~~soil~surface          
                  ||  SLURRY   ||
                  ||           ||Lower~wall--SOIL
		  ===============   
                       Floor
                         |
                       SOIL
```

The `~~~~~~~~~~` line on the right shows the soil surface while `^^^^^^^^^^` represents the surface of the slurry.
Double lines `||` and `===` represent the storage structure walls. 
The temperature of the components in CAPS determine heat transfer rates, while others influence only resistance to heat transfer.
So, for example, heat transfer through the floor is determined by the sum of slurry, floor, and soil resistance, and the temperature difference between the soil (not the floor) and slurry.

The model is effectively zero-dimensional, using a lumped capacitance approach.
Slurry loses (or gains) heat through the floor and walls of the structure, each of which have a single temperature and cross-sectional area at any given time step.
Temperatures are based on very simple predictions of soil temperature at the floor depth or the middle of the buried wall depth.
There is no effect of heat loss or gain on the environment--soil does not warm or cool due to interaction with slurry.
Heat transfer resistance are essentially "overall" values that include multiple modes of heat transfer, with units of K-m2/W.
For the floor and wall components, heat flux (Q, W/m2) within a time step is therefore:

```
Q = (T(slurry) - T(environment)) / R * A
```

where R is the overall resistance term, and A is the cross-sectional area available for heat transfer.
The values of A change for the wall components as the slurry level changes.
The overall resistance term for any route is simply the sum of relevant terms.
The model uses the following expressions.

```
  R(floor) = R(slurry) + R(wall) + R(soil)
  R(dwall) = R(slurry) + R(wall) + R(soil)
  R(uwall) = R(slurry) + R(wall) + R(air)
  R(top) =   R(slurry) + R(air) 
```

The resistance term for air, like all others for individual components, is calculated externally by the user, and therefore could include the effect of a cover.
Similarly, insulation could be added to the wall, floor, or top resistance terms.
Any resistance from the temperature gradient that forms in slurry is included in the slurry resistance term.

Radiation, taken as the product of solar radiation and slurry surface absorptivity, can only increase slurry temperature.
Because the model simulates *average* slurry temperature, and does not explicitly include heat loss by radiation, the absorptivity value is effectively a net value.
Actual absorptivity may be higher--the surface may absorb a greater fraction of the incident radiation.
But some of this heat will be lost to the air and through other routes, to a larger degree than reflected in model results, due to surface warming.
Radiation may also drive evaporation.
This heat energy consumed by the phase change does not go toward warming slurry, and so should not be expected to be included in the absorptivity value.

Slurry addition may add heat energy.
Feed flow rate is constant over a simulation or (if a slurry level file is used) at least a daily basis.
Heat transfer by slurry addition can be turned off, effectively assuming the incoming slurry has the same temperature as stored slurry.
This a convenient when its temperature is unknown or variable.
Slurry removal is intermittent.
It has no immediate effect on slurry temperature, but of course reduces the slurry mass and therefore the heat energy content of the stored slurry.

## Numerical solution
A simple first-order fixed time step approach is used.
The time step is one hour, although weather is taken as constant over each day.
To avoid numerical instability (probably only a problem when slurry mass (depth) is very low), a steady-state temperature is calculated at each time step, and taken as the slurry temperature if rate calculations suggest that the slurry temperature should surpass the steady-state value.

## Freezing
Freezing and thawing of slurry are included, mainly to capture the behavior of slurry around a temperature of 0 degrees C.
However, the same is not true for soil.
The mass of frozen slurry is tracked separately from the total slurry mass.
During periods of cooling, slurry temperature may drop below 0 degrees C only once it has completely frozen.
Conversely, during warming, frozen slurry cannot exceed 0 degrees C; any heat transferred into the slurry is first used for the phase change.

## Weather
Weather may be given in an input file as daily average air temperature and solar radiation by the day of the year.
Or, minimum, maximum, and day of year of the maximum be specified in the "user parameter" file, and used internally with a sine function to estimate daily values.
With either approach, weather inputs are daily; the model does not accept nor simulate hourly weather.

## Missing components
The model does not include:

* Explicit heat loss by radiation (but see information on radiative heating above).
* Any effects of precipitation (although specification of slurry level directly and acceptance that absorptivity excludes radiation that goes toward evaporation may address this limitation)
* Soil freezing/thawing
* Effects of the slurry tank on soil temperature

There are no plans to include any of these at the moment; some are impractical to include in such a simple model.



