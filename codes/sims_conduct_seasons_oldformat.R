for (s in 2019:2025){
  

  #if want to repeat for robustness check
  #then also set robustness=TRUE
  #for (re in 1:10){
  
  library(tidyverse)
  library(data.table)
  library(readxl)
  library(writexl)
  source("codes/forecast_functions_randomdraw.R")

  season <- s
  #seed real or elo or pre2015
  seed <- "pre2015"
  seed <- "elo"
  
  # #code to get the pots that was actually used in the sims-------------
  # d <- read.delim(paste0("draw_effect/data/Groups_UEFA_CL/Groups_UEFA_CL/Groups_", season, "_", season+1, ".txt"),
  #                 sep=",", fileEncoding = "UTF-8") |>
  #   arrange(desc(ClubCoeff)) |>
  #   arrange(desc(CL_winner)) |>
  #   mutate(Pot_new_real=rep(1:4, each=9)) |>
  #   arrange(desc(ClubElo)) |>
  #   mutate(Pot_new_elo=rep(1:4, each=9))
  # 
  # #modified for 2025
  # d2 <- d |> filter(Pot!=-1) |>
  #   arrange(desc(ClubCoeff)) |>
  #   arrange(desc(CL_winner)) |>
  #   mutate(Pot_old_real=rep(1:4, each=8)) |>
  #   arrange(desc(ClubElo)) |>
  #   mutate(Pot_old_elo=rep(1:4, each=8)) |>
  #   full_join(d)

  #write_xlsx(d2, paste0("draw_effect/data/Pots_years/pots_", season, ".xlsx"))

  draws_map <- read.delim(paste0("draw_effect/data/Groups_UEFA_CL/Groups_UEFA_CL/Groups_", season, "_", season+1, ".txt"), 
                          sep=",", fileEncoding = "UTF-8") |> 
    select(Team=Club, Club=Index, Country, Elo=ClubElo, Pot)
  draws <- read.delim(paste0("draw_effect/data/Groups_UEFA_CL/Draw_group_stage_", seed, "/", season, "_", season+1, ".txt"),
                      sep=",") %>% rename(Club=Team) |> 
    #pivot_longer(cols=X1H:X4A, values_to= "Opp") %>% 
    #separate(name, into=c("OppPot", "Ground"), sep=2) %>% 
    left_join(draws_map) |> 
    mutate(simnr_draw=Draw+1) |> 
    select(-Club, -Draw) |> 
    rename(group=Group, Club=Team)
  
  #run the sims for each draw------
  
  #create allopps and allmatches format for every sim
  allopps <- draws %>%
    inner_join(draws, by = c("group", "simnr_draw"), suffix = c("", "_opp"), relationship = "many-to-many") %>%
    filter(Club != Club_opp) %>%
    mutate(Ground="h") %>% 
    bind_rows(draws %>%
                inner_join(draws, by = c("group", "simnr_draw"), suffix = c("", "_opp"), relationship = "many-to-many") %>%
                filter(Club != Club_opp) %>%
                mutate(Ground="a")) %>% 
    rename(Opp=Club_opp, Elo.x=Elo, Elo.y=Elo_opp)
  
  
  ########IF WANT TO REPLACE WITH AVG ELO OF GROUP - Need to change for old seasons - but Karel has the pots there
  
  # allopps <- allopps |> 
  #   left_join(allopps |> group_by(Pot) |> summarise(PotmeanElo=mean(Elo.x)), by=c("Pot"="Pot")) |> 
  #   left_join(allopps |> group_by(Pot) |> summarise(PotmeanElo=mean(Elo.x)), by=c("Pot_opp"="Pot")) |> 
  #   mutate(Elo.x=PotmeanElo.x, Elo.y=PotmeanElo.y)
  # %>% 
  
  # #temporarily set simnr
  # mutate(simnr_draw=i)
  
  ##########continue
  allmatches <- allopps %>% transmute(Home=ifelse(Ground=="h", Club, Opp),
                                      Away=ifelse(Ground=="a", Club, Opp),
                                      Elo_h=ifelse(Ground=="h", Elo.x, Elo.y),
                                      Elo_a=ifelse(Ground=="a", Elo.x, Elo.y),
                                      elodiff=Elo_h-Elo_a,
                                      simnr_draw = simnr_draw
  ) %>% unique()
  
  
  #do the simulation
  load("clubelo_data/poissoncoefs.rda")
  
  setDT(allmatches)
  setDT(allopps)
  
  rankings_list <- list()
  simlength <- 1000
  
  for (i in 1:simlength){
    results <- sim_remaining(allmatches, pcoefs)
    results_byteam <- sim_byteam(allopps, results)
    league_table <- sim_table_oldcl(results_byteam)
    ranking <- league_table %>% mutate(adv16=ifelse(rank<=2, 1, 0))
    rankings_list[[i]] <- ranking
    print(i)
  }
  
  allsims <- rbindlist(rankings_list)
  setDT(allsims)
  
  #set if want to use robustness check
  robustness <- FALSE

  if (robustness==TRUE) {
    saveRDS(allsims, file = paste0("draw_effect/data/sims/robustness/old_allsims_1000x1000", seed, "_", season, "_", re, ".rds"), compress = "gzip")
  } else {
    saveRDS(allsims, file = paste0("draw_effect/data/sims/old_allsims_1000000_", seed, "_", season, ".rds"), compress = "gzip")
  }
  
 
  #aggregate the results
  
  # Step 1: Summarize 'n' by Club and rank
  club_rank_counts <- allsims[, .(
    n = .N  # Count occurrences of each Club and rank
  ), by = .(Club, rank)]
  
  # Step 2: Pivot wider with ranks as columns
  club_rank_wide <- dcast(club_rank_counts, Club ~ rank, value.var = "n", fill = 0)
  
  # Step 3: Calculate Elo and Prob_Round16, joining by Club
  club_elo_prob <- allsims[, .(
    Elo = first(Elo),  # Get the first Elo value for each Club
    Prob_Round16 = mean(adv16)  # Calculate mean of adv16 for each Club
  ), by = Club]
  
  # Step 4: Calculate Avg_opponent_Elo, joining by Club
  avg_opponent_elo <- allopps[, .(
    Avg_opponent_Elo = mean(Elo.y)  # Mean of opponent Elo values
  ), by = Club]
  
  # Step 5: Perform the final joins to create the out_table
  out_table <- club_rank_wide[
    club_elo_prob, on = "Club"][
      avg_opponent_elo, on = "Club"] |> left_join(draws_map, by=c("Club"= "Team", "Elo"))
  
  if (robustness==TRUE){
    print(paste("repeat", re))
    write_xlsx(out_table, paste0("draw_effect/data/out_tables/robustness/old_out_table_1000x1000", seed, "_", season, "_", re, ".xlsx"))
  } else {
    write_xlsx(out_table, paste0("draw_effect/data/out_tables/old_out_table_1000000_", seed, "_", season, ".xlsx"))
    
  }
  
  rm(list = ls())
  gc()
}

#}

