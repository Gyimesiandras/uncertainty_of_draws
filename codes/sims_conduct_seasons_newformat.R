
for (s in 2019:2025){

#if want to repeat for robustness check
#then also set robustness=TRUE
#for (re in 1:100){
  
  library(tidyverse)
  library(data.table)
  library(readxl)
  library(writexl)
  source("codes/forecast_functions_randomdraw.R")
  
season <- s
#seed real or elo
seed <- "real"
seed <- "elo"

#read the draws
  draws_map <- read.delim(paste0("draw_effect/data/Groups_UEFA_CL/Groups_UEFA_CL/Groups_", season, "_", season+1, ".txt"), 
                          sep=",", fileEncoding = "UTF-8") |> 
    select(Team=Club, Club=Index, Country, Elo=ClubElo, Pot)
  draws <- read.delim(paste0("draw_effect/data/Groups_UEFA_CL/Draw_iRR_", seed, "/", season, "_", season+1, ".txt"),
                             sep=",") %>% 
    pivot_longer(cols=X1H:X4A, values_to= "Opp") %>% 
    separate(name, into=c("OppPot", "Ground"), sep=2) %>% 
    left_join(draws_map, by="Club") %>% 
    left_join(draws_map, by=c("Opp"="Club"))
  allopps <- draws %>% mutate(simnr_draw=draw+1, Ground=tolower(Ground)) |> 
    select(-Opp, -Club, - draw) |> 
    rename(Club=Team.x, Opp=Team.y)
  
  

###############IF WANT TO REPLACE WITH AVG ELO OF GROUP
# pool <- data.frame(Club = c(Pot1, Pot2, Pot3, Pot4), Pot = c(rep(1, length(Pot1)), rep(2, length(Pot2)), rep(3, length(Pot3)), rep(4, length(Pot4))))
# allopps <- allopps |> 
#   left_join(pool |> select(Club, Pot), by=c("Club"="Club")) |> 
#   left_join(pool |> select(Club, Pot), by=c("Opp"="Club"))
# 
# allopps <- allopps |> 
#   left_join(allopps |> group_by(Pot.x) |> summarise(PotmeanElo=mean(Elo.x)), by=c("Pot.x"="Pot.x")) |> 
#   left_join(allopps |> group_by(Pot.x) |> summarise(PotmeanElo=mean(Elo.x)), by=c("Pot.y"="Pot.x")) |> 
#   mutate(Elo.x=PotmeanElo.x, Elo.y=PotmeanElo.y)
# 

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

#set if want to use robustness check
robustness <- FALSE
iterations <- 50

if (robustness) {
  am <- allmatches[1:144,]
  ao <- allopps[1:288,]
  
  allmatches <- do.call(rbind, lapply(1:iterations, function(i) {
    am$simnr_draw <- i
    am
  }))
  
  allopps <- do.call(rbind, lapply(1:iterations, function(i) {
    ao$simnr_draw <- i
    ao
  }))
}

# do the simulations
rankings_list <- list()
for(i in 1:1000){
  results <- sim_remaining(allmatches, pcoefs)
  results_byteam <- sim_byteam(allopps, results)
  league_table <- sim_table(results_byteam)
  playoff_dt <- sim_playoff(league_table)
  adv16 <- bind_rows(playoff_dt %>% select(Club=won, simnr_draw),
                     league_table %>% filter(rank <= 8) %>% select(Club, simnr_draw)) %>% 
    mutate(adv16=1)
  ranking <- league_table %>% left_join(adv16, by=c("simnr_draw", "Club"))
  ranking$adv16[is.na(ranking$adv16)] <- 0
  rankings_list[[i]] <- ranking
  print(i)
}


allsims <- rbindlist(rankings_list)
setDT(allsims)
if (robustness==TRUE) {
  saveRDS(allsims, file = paste0("draw_effect/data/sims/robustness/new_allsims_1000x1000", seed, "_", season, "_", re, ".rds"), compress = "gzip")
} else {
  saveRDS(allsims, file = paste0("draw_effect/data/sims/new_allsims_1000000_", seed, "_", season, ".rds"), compress = "gzip")
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
  write_xlsx(out_table, paste0("draw_effect/data/out_tables/robustness/new_out_table_1000x1000", seed, "_", season, "_", re, ".xlsx"))
} else {
  write_xlsx(out_table, paste0("draw_effect/data/out_tables/new_out_table_1000000_", seed, "_", season, ".xlsx"))
  
}

rm(list = ls())
gc()
}

#}
