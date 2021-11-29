# Information on running examples

# Windows
## A. Double-click exe
If you double-click the file stm.exe, the program will run with default inputs.
This will only work as intedned in the case of example 3, where both input files have default names (pars.txt and user_pars.txt).
For the other examples, you should see an error returned if you try this method.
This option will create output files with the ID 0001 (the default).

## B. Working with a batch file
Each of the examples has a batch file with the correct command for running the example, names `run.bat`.
You can open, view, or edit each file using a text editor. 
For example 1, the contents are:

```
REM Batch file to run example 1 on Windows
REM This example uses measured weather and slurry level

.\stm.exe 0001 pars.txt Back_u_pars.txt Uppsala_weather.txt Back_level.txt
```

This file can be run by double-clicking.
Or, you can open Command Prompt (or Power Shell), `cd` to the directory, and then run the batch file at the command line.
For example:

```
C:\Users\sasha>cd Git-repos\STM\applications\CH4_2021\examples\01

C:\Users\sasha\Git-repos\STM\applications\CH4_2021\examples\01>.\run.bat
```

# Linux
Each of the three examples can be run in Linux by running the script `run.sh` in a shell, or copying the command from it and pasting it directly in a shell.
To run the shell script `run.sh`, it may be necessary to add execute permission (with `chmod`), and a preceding `./` is probably needed.

```
\.run.sh
```

# Input files
Each example includes at least 2 input files.
The main parameter file has a default name of pars.txt.
The "user parameter" file has a default name of user_pars.txt.
Parameters are described within the files.
Any value can be changed to see the effect on results--just be sure to keep the line spacing constant and to save the file before running the model.

# Output
The program creates three output files: `ID_rates.txt`, `ID_temp.txt`, and `ID_weather.txt`, where `ID` = the run ID provided in the command for running the model. 
Slurry temperature is in the `temp` file in the last column.
All files are space-delimited, and could be opened in any text editor or spreadsheet program.
