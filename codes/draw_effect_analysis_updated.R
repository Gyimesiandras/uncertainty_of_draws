library(tidyverse)
library(readxl)
library(data.table)
library(writexl)

#read data
#set the season
season <- 2023
#could be real/pre2015, elo
seed <- "real"
seed_old <- "pre2015"

r_cl <- read_excel(paste0("draw_effect/data/out_tables/new_out_table_1000000_", seed,"_", season,  ".xlsx"))
allsims_new <- readRDS(paste0("draw_effect/data/sims/new_allsims_1000000_", seed,"_", season,  ".rds"))
r_cl_old <- read_excel(paste0("draw_effect/data/out_tables/old_out_table_1000000_", seed_old,"_", season,  ".xlsx"))
allsims_old <- readRDS(paste0("draw_effect/data/sims/old_allsims_1000000_", seed_old,"_", season,  ".rds"))

#could be empty or noplayoff
po <- "noplayoff"

#bootstrap CL
B <- 1000

#sampling for the robustness checks, could be TRUE or FALSE, and set the sample nr - if doing bootstrapping this is not needed, so set FALSE
sampling <- FALSE
sn <- 500
iterations <- 10
samples_list <- data.frame()

#for(it in 1:iterations){

#start the calculations for the new format
allsims_random <- as.data.table(allsims_new)

if(sampling){
  samp <- sample(1:1000, sn)
  allsims_random <- allsims_random[simnr_draw %in% samp,]
}

#if want to remove the plaoyoff
if (po=="noplayoff"){
  allsims_random <- as.data.table(allsims_random |> mutate(adv16=ifelse(rank<=16, 1, 0)))
}


#there are 1000 different draws (simnr_draw), we calculate the avg prob (adv16) for each
adv16_ratios <- allsims_random[, .(Prob_Round16_sim = mean(adv16)), by = c("simnr_draw", "Club")]

adv16_sum_byclub_newcl <- adv16_ratios[
  ,
  {
    x <- adv16_ratios$Prob_Round16_sim
    
    # observed SD
    sd_hat <- sd(x)
    
    # bootstrap SDs
    boot_sds <- replicate(B, {
      sd(sample(x, replace = TRUE))
    })
    
    .(
      mean_Prob_Round16 = mean(x),
      sd_Prob_Round16   = sd_hat,
      max_Prob_Round16  = max(x),
      min_Prob_Round16  = min(x),
      iqr_Prob_Round16  = IQR(x),
      
      lower_95_sd = quantile(boot_sds, 0.025),
      upper_95_sd = quantile(boot_sds, 0.975)
    )
  },
  by = Club
]


#start the old format data--------------
allsims_random <- as.data.table(allsims_old)

if(sampling){
  samp <- sample(1:1000, sn)
  allsims_random <- allsims_random[simnr_draw %in% samp,]
}

#there are 1000 different draws (simnr_draw), we calculate the avg prob (adv16) for each
adv16_ratios <- allsims_random[, .(Prob_Round16_sim = mean(adv16)), by = c("simnr_draw", "Club")]
adv16_ratios <- merge(adv16_ratios, r_cl_old %>% select(Club, Prob_Round16))
adv16_ratios_old <- adv16_ratios[, Prob_Round16_diff := abs(Prob_Round16_sim-Prob_Round16)]
adv16_summary <- adv16_ratios_old[, .(max_Prob_Round16_diff = max(Prob_Round16_diff)), by= simnr_draw]

adv16_sum_byclub_old <- adv16_ratios_old[
  ,
  {
    x <- Prob_Round16_sim
    
    # observed SD
    sd_hat <- sd(x)
    
    # bootstrap SDs
    boot_sds <- replicate(B, {
      sd(sample(x, replace = TRUE))
    })
    
    .(
      mean_Prob_Round16 = mean(x),
      sd_Prob_Round16   = sd_hat,
      max_Prob_Round16  = max(x),
      min_Prob_Round16  = min(x),
      iqr_Prob_Round16  = IQR(x),
      
      lower_95_sd = quantile(boot_sds, 0.025),
      upper_95_sd = quantile(boot_sds, 0.975)
    )
  },
  by = Club
]


#compare the data for each teams:
data_comp <- r_cl %>% left_join(r_cl_old, by="Club") %>% 
  select(Club, Prob_Round16.x, Prob_Round16.y, Elo.x, Elo.y, Avg_opponent_Elo.x, Avg_opponent_Elo.y, Pot_old=Pot.y, Pot_new=Pot.x)

#compare the probabilities
probs_comp <- adv16_sum_byclub_newcl %>% mutate(format="new") %>% 
  bind_rows(adv16_sum_byclub_old %>% mutate(format="old")) |> 
  left_join(data_comp |> select(Club, Elo=Elo.x, Pot_old, Pot_new))

#this part is for an old manual version-----------
# #old format
# #with seeding the national champs
# pot1 <- c("Real Madrid", "Atalanta", "Manchester City", "Bayer Leverkusen", "Inter Milan", "Paris Saint-Germain", "PSV Eindhoven", "Sporting CP")
# pot2 <- c("Bayern Munich", "Liverpool", "RB Leipzig", "Barcelona", "Atlético Madrid", "Juventus", "Benfica", "Arsenal")
# pot3 <- c("Club Brugge", "Shakhtar Donetsk", "Milan", "Feyenoord", "Dinamo Zagreb", "Red Bull Salzburg", "Red Star Belgrade", "Young Boys")
# #https://en.wikipedia.org/wiki/2024%E2%80%9325_UEFA_Champions_League_league_phase
# 
# #without seeding the nat champs, only using UEFA coefficients
# pot1 <- c("Real Madrid", "Manchester City", "Bayern Munich", "Inter Milan", "Paris Saint-Germain", "Liverpool", "RB Leipzig", "Barcelona")
# pot2 <- c("Bayer Leverkusen", "Atalanta", "Club Brugge", "Atlético Madrid", "Juventus", "Benfica", "Arsenal", "Shakhtar Donetsk")
# pot3 <- c("PSV Eindhoven", "Sporting CP", "Milan", "Feyenoord", "Dinamo Zagreb", "Red Bull Salzburg", "Red Star Belgrade", "Young Boys")
# 
# #with perfect seeding by Elo
# pot1 <- r_cl_old |> slice_max(Elo, n=8) |> pull(Club)
# pot2 <- r_cl_old |> 
#   slice_max(Elo, n = 16) |> 
#   slice(9:16) |> 
#   pull(Club)
# pot3 <- r_cl_old |> 
#   slice_max(Elo, n = 24) |> 
#   slice(17:24) |> 
#   pull(Club)
# 
# 
# #create a figure - color= pots
# probs_comp <- probs_comp %>% mutate(Pot_old=case_when(
#   Club %in% pot1 ~ 1,
#   Club %in% pot2 ~ 2,
#   Club %in% pot3 ~ 3,
#   TRUE ~ 4
# )) %>%  left_join(
#   read_csv2(file="clubelo_data/allopps.csv", locale = locale(encoding = "Latin1")) %>% select(Club, Pot_new=Pot) %>% unique()
# ) %>% 
#   mutate(Pot=as.factor(ifelse(format=="old", Pot_old, Pot_new))) %>% 
#   left_join(r_cl %>%  select(Club, Elo))
# # a Pot new nem feltétlen stimmel de nem baj, mert az old van az ábrán

#scatter plot
probs_comp_wide <- probs_comp |> pivot_wider(id_cols=c(Club, Pot_new, Pot_old, Elo), 
                                             names_from = format, 
                                             values_from=c("mean_Prob_Round16", "sd_Prob_Round16", "lower_95_sd", "upper_95_sd"))
if (sampling){
  samples_list <- samples_list |> bind_rows(probs_comp_wide)
} else {
  saveRDS(probs_comp_wide, file=paste0("draw_effect/data/comparison/comparison_", seed,po,"_", season, ".rds"))
}

#}

