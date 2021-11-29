REM Batch file to run example 1 on Windows
REM This example uses calculated weather and calculated slurry level based on a fixed addition rate and 2 removal events per year
REM Note that the user parameter file name has been changed to the default of user_pars.txt--this was done so the example can be run by double-clicking stm.exe instead of using the command below or this script (but the ID 0001 will be used for output files)

.\stm.exe 0003 pars.txt user_pars.txt
