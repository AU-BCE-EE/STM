# Runs simulations and makes plots

sim_start_time="$SECONDS"

# Run all simulations in parallel
./stm Back pars/pars.txt pars/Back_user_pars.txt weather/Uppsala_weather.txt &
./stm Fitt pars/pars.txt pars/Fitt_user_pars.txt weather/Uppsala_weather.txt &
./stm Lind pars/pars.txt pars/Lind_user_pars.txt weather/Uppsala_weather.txt &
./stm Raan pars/pars.txt pars/Raan_user_pars.txt weather/Uppsala_weather.txt &
./stm Tjel pars/pars.txt pars/Tjel_user_pars.txt weather/Tjele_weather.txt 

sim_end_time="$SECONDS"

# Move output
mv *_temp.txt* stm_output
mv *_weather* stm_output
mv *_rates* stm_output

# Run R scripts

R CMD BATCH --nosave --norestore 'plot.R'

rm Rplots.pdf
rm plot.Rout
rm .RData

plot_end_time="$SECONDS"

echo "Model runs took $(($sim_end_time-$sim_start_time)) seconds."

echo "Plotting took $(($plot_end_time-$sim_end_time)) seconds."

