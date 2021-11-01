# Runs simulations and makes plots

sim_start_time="$SECONDS"

# Run all simulations in parallel
./stm Back pars/pars.txt pars/Back_user_pars.txt &
./stm Fitt pars/pars.txt pars/Fitt_user_pars.txt &
./stm Lind pars/pars.txt pars/Lind_user_pars.txt &
./stm Raan pars/pars.txt pars/Raan_user_pars.txt

sim_end_time="$SECONDS"

# Move output
mv *temp.txt* stm_output
mv *weather* stm_output
mv *rates* stm_output

# Run R scripts

R CMD BATCH --nosave --norestore 'plot.R'

plot_end_time="$SECONDS"

echo "Model runs took $(($sim_end_time-$sim_start_time)) seconds."

echo "Plotting took $(($plot_end_time-$sim_end_time)) seconds."

