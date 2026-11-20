# Content

- data/Groups_UEFA_CL folder contains 1000 simulated Champions League (CL) draws for all 4 different formats: group stage seeded by Elo, group stage seeded by UEFA coefficients (real pre2015/post2024 rule), incomplete round robin (iRR) seeded by Elo, and iRR seeded by UEFA coefficient (real pre2015/post2024 rule). Indices (Club) are used as identifiers.
- data/Groups_UEFA_CL/Groups_UEFA_CL contains the set of participants from seasons 2019/20 to 2025/26 used in the simulations and data on their UEFA coefficient (ClubCoeff), Elo rating on 1st September right before the corresponding season (ClubElo) and their indices used as identifiers (Index).
- data/Pots_years folder contains the same information in more readable Excel files, as well as the pot allocation under the 2 old (group stage) and 2 new (iRR) format.
- data/comparison folder contains the results of the simulations in .rds (R data) files for 3 different scenarios of the old and new format: real scenario (real pre2015/post2024 seeding), elo scenario (Elo seeding) and elonoplayoff scenario (Elo seeding with the best 16 directly advancing through the league phase without a playoff in the new iRR format). Observation are the teams. Variables are the mean and standard deviation (sd) and its bootstrapped confidence intervals of the probabilites of reaching the Round of 16 under the 1000 different draws.
- data/comparison/robustness folder contains robustness checks for the number of simulations.
  
- codes/forecast_functions_randomdraw.R compiles R functions used for simulations.
- codes/sims_conduct_seasons_newformat.R contains the R code to conduct the simulations for the new iRR format.
- codes/sims_conduct_seasons_newformat.R contains the R code to conduct the simulations for the old group stage format.
- codes/draw_effect_analysis_updated.R contains the R code to read the simulation results and calculate aggregate measures. This produces the file in the data/comparison folder.
- codes/draw_eff_visual.R contains the code to transform the data into what is needed for the figures, and creating ggplot figures.
