library(dplyr)
library(data.table)

# Vectorized version of simulating remaining league stage matches
sim_remaining <- function(remaining, p, incentive_modifier=FALSE, eta=0) {
  res <- copy(remaining)
  we_vec <- 1 / (1 + 10^(-remaining$elodiff / 400))
  
  xgh_vec <- p$hc[1] + p$hc[2] * we_vec + p$hc[3] * we_vec^2 + p$hc[4] * we_vec^3
  xga_vec <- p$ac[1] + p$ac[2] * we_vec + p$ac[3] * we_vec^2 + p$ac[4] * we_vec^3
  
  if (incentive_modifier){
    xgh_vec <- xgh_vec * (1/(1+res$iota*eta))*(1+ eta*((res$possible_gain.x - res$possible_loss.x)+(res$possible_gain.y - res$possible_loss.y)) )
    xga_vec <- xga_vec * (1/(1+res$iota*eta))*(1+ eta*((res$possible_gain.x - res$possible_loss.x)+(res$possible_gain.y - res$possible_loss.y)) )
  }
  
  gh_vec <- rpois(n = nrow(remaining), lambda = xgh_vec)
  ga_vec <- rpois(n = nrow(remaining), lambda = xga_vec)
  
  won_vec <- ifelse(gh_vec > ga_vec, "Home", ifelse(gh_vec < ga_vec, "Away", "Draw"))
  
  res[, `:=`(we = we_vec, xgh = xgh_vec, xga = xga_vec, gh = gh_vec, ga = ga_vec, won = won_vec)]
  return(res)
}

# Vectorized sim_byteam
sim_byteam <- function(allopps, results_dt) {

    home_results <- allopps[Ground == "h"][results_dt, on = .(Club = Home, Opp = Away, simnr_draw = simnr_draw)]
  home_results[, `:=`(goals = gh, goals_against = ga,
                      points = fifelse(won == "Home", 3, fifelse(won == "Draw", 1, 0)))]
  
  away_results <- allopps[Ground == "a"][results_dt, on = .(Club = Away, Opp = Home, simnr_draw = simnr_draw)]
  away_results[, `:=`(goals = ga, goals_against = gh,
                      points = fifelse(won == "Away", 3, fifelse(won == "Draw", 1, 0)))]
  
  combined_results <- rbindlist(list(home_results, away_results))
  return(combined_results)
}

# Vectorized sim_byteam (alternative version without simnr_draw in the data)
sim_byteam_nosimnr <- function(allopps, results_dt) {
  
  home_results <- allopps[Ground == "h"][results_dt, on = .(Club = Home, Opp = Away)]
  home_results[, `:=`(goals = gh, goals_against = ga,
                      points = fifelse(won == "Home", 3, fifelse(won == "Draw", 1, 0)))]
  
  away_results <- allopps[Ground == "a"][results_dt, on = .(Club = Away, Opp = Home)]
  away_results[, `:=`(goals = ga, goals_against = gh,
                      points = fifelse(won == "Away", 3, fifelse(won == "Draw", 1, 0)))]
  
  combined_results <- rbindlist(list(home_results, away_results))
  return(combined_results)
}

# Vectorized sim_table
sim_table <- function(results_byteam, missing_simnr_draw=TRUE) {
  if (!("simnr_draw" %in% names(results_byteam))) {
    results_byteam[, simnr_draw := 1L]
    added_simnr_draw <- TRUE
  } else {
    added_simnr_draw <- FALSE
  }
  
  league_table <- results_byteam[, .(
    Points = sum(points),
    Goals = sum(goals),
    GoalsAgainst = sum(goals_against),
    GoalDiff = sum(goals) - sum(goals_against),
    GoalsAway = sum(ifelse(Ground == "a", goals, 0), na.rm = TRUE),
    Wins = sum(points == 3),
    WinsAway = sum(ifelse(Ground == "a", points == 3, 0), na.rm = TRUE),
    Elo=first(Elo.x)
  ), by = c("simnr_draw", "Club")]
  
  setkey(results_byteam, Opp, simnr_draw)
  setkey(league_table, Club, simnr_draw)
  
  opp_table <- league_table[results_byteam, on = .(Club = Opp, simnr_draw)]
  
  opp_table <- opp_table[, .(
    OppPoints = sum(Points),
    OppGoalDiff = sum(GoalDiff),
    OppGoals = sum(Goals)
  ), by = c("simnr_draw", "i.Club")]
  
  league_table <- opp_table[
    league_table,
    on = .(simnr_draw, i.Club=Club)
  ]
  setnames(league_table, "i.Club", "Club")
  
  setorder(league_table, simnr_draw, -Points, -GoalDiff, -Goals, -GoalsAway, -Wins, -WinsAway, -OppPoints, -OppGoalDiff, -OppGoals)
  league_table[, rank := seq_len(.N), by=.(simnr_draw)]

  
  if (added_simnr_draw) {
    league_table[, simnr_draw := NULL]
  }
  
  return(league_table)
}

# Vectorized sim_table old cl
sim_table_oldcl <- function(results_byteam) {
  league_table <- results_byteam[, .(
    Points = sum(points),
    Goals = sum(goals),
    GoalsAgainst = sum(goals_against),
    GoalDiff = sum(goals) - sum(goals_against),
    GoalsAway = sum(ifelse(Ground == "a", goals, 0), na.rm = TRUE),
    Wins = sum(points == 3),
    WinsAway = sum(ifelse(Ground == "a", points == 3, 0), na.rm = TRUE),
    Elo=first(Elo.x),
    Group=first(group)
  ), by = c("simnr_draw", "Club")]
  
  opp_table <- results_byteam %>% 
    left_join(league_table[, .(simnr_draw, Club, Points)], by=c("Club"="Club", "simnr_draw"="simnr_draw")) %>% 
    left_join(league_table, by=c("Opp"="Club", "simnr_draw"="simnr_draw"))
  opp_table <- opp_table[Points.x==Points.y, .(
    Group=first(Group),
    pointstied = sum(points),
    goaldifftied = sum(goals-goals_against),
    goalsFtied = sum(goals)
  ), by = c("simnr_draw", "Club")]
  opp_table[, rank1 := frank(list(-pointstied, -goaldifftied, -goalsFtied), ties.method="min"), by = .(simnr_draw, Group)]
  
  
  league_table <- opp_table[, .(simnr_draw, Club, rank1)][
    league_table,
    on = .(simnr_draw, Club)
  ]
  
  setorder(league_table, simnr_draw, Group, -Points, rank1, -GoalDiff, -Goals)
  league_table[, rank := seq_len(.N), by=.(simnr_draw, Group)]

  return(league_table)
}

#simulate playoff round
sim_playoff <- function(all_table){
  playoff_list <- list()
  for (sim in 1:(nrow(all_table)/36)){
    table=all_table[simnr_draw==sim]
    s1=c(sample(17:18) , sample(19:20) ,sample(21:22) ,sample(23:24))
    s2=c(sample(15:16) , sample(13:14) ,sample(11:12) ,sample(9:10))
    
    playoff <- data.frame(Club1=table$Club[s1],
                          Club2=table$Club[s2],
                          Elo1=table$Elo[s1],
                          Elo2=table$Elo[s2]) %>% 
      mutate(           elodiff=Elo1-Elo2,
                        we2=1/(1+10^(-sqrt(2)*elodiff/400)),
                        rnd=runif(8),
                        won=case_when(rnd < we2 ~ Club1,
                                      TRUE ~ Club2
                        ),
                        simnr_draw=sim
                        
      )
    playoff_list[[sim]] <- playoff
  }
  playoff_dt <- rbindlist(playoff_list)
  return(playoff_dt)
}
