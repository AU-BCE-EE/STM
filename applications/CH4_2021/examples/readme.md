# Information on running examples
Each of the three examples can be run in Linux by running the script `run.sh` in a shell, or copying the command from it and pasting it directly in a shell.
To run the shell script `run.sh`, it may be necessary to add execute permission, and a preceding `./` is probably needed.

```
\.run.sh
```

The program creates three output files: `ID_rates.txt`, `ID_temp.txt`, and `ID_weather.txt`, where `ID` = the run ID provided in the command for running the model. 
Slurry temperature is in the `temp` file in the last column.
All files are space-delimited.

# Windows
To run on Windows, the source code file `stm.f90` (see root directory) must be compiled.
With the GNU compiler, this could be done in Command Prompt with:

```
gfortran stm.f90 -o stm.exe
```

The commands in the shell scripts would need to be updated to reflect the new executible name.
For example 3:

```
stm.exe 0003 pars.txt Back_u_pars.txt
```

Similar to Linux, this command could be pasted directly in Command Prompt, or saved in a Batch file, e.g., `run.bat`.


