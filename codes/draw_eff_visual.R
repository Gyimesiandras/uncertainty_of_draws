
library(tidyverse)
library(readxl)
library(data.table)
library(ggplot2)
library(readr)
library(writexl)
library(forcats)
library(ggrepel)
library(scales)

#run draw_effect_analysis_updated.R first
#set the season
s <- 2024

#could be "" or "sampling"
sampling <- ""
pots <- read_excel(paste0("draw_effect/data/Pots_years/pots_", s, ".xlsx")) |> 
  select(Club, Pot_old_real, Pot_new_real,Pot_old_elo, Pot_new_elo,)

official <- readRDS(paste0("draw_effect/data/comparison/", sampling, "/comparison_real_", s, ".rds")) |> mutate(scenario="official") |> 
  left_join(pots) |> 
  mutate(Pot_new=Pot_new_real,
         Pot_old=Pot_old_real)
perf <- readRDS(paste0("draw_effect/data/comparison/", sampling, "/comparison_elo_", s, ".rds")) |> mutate(scenario="perfectseeding") |> 
  left_join(pots) |> 
  mutate(Pot_new=Pot_new_elo,
         Pot_old=Pot_old_elo)
noplayoff <- readRDS(paste0("draw_effect/data/comparison/", sampling, "/comparison_elonoplayoff_", s, ".rds")) |> mutate(scenario="noplayoff") |> 
  left_join(pots) |> 
  mutate(Pot_new=Pot_new_elo,
         Pot_old=Pot_old_elo)


probs_comp_wide <- official

ggplot(data=probs_comp_wide |> filter(!is.na(Pot_old_real)), aes(x=mean_Prob_Round16_old, y=mean_Prob_Round16_new)) +
  geom_label_repel(aes(label=Club), size=3, max.overlaps=100) + 
  geom_point(aes(color=as.factor(Pot_old)), size=2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray20") +
  labs(
    x = "Probability of reaching Round of 16 (old format)",
    y = "Probability of reaching Round of 16 (new format)",
    color = "Old format Pot",
    title = "Comparing old vs. new format probabilities (before the draw)",
  ) + scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position="bottom")

#ggsave(file="draw_effect/Probs_oldvsnew_scatter_2025.pdf", scale=1.2)

ggplot(data=probs_comp_wide |> filter(!is.na(Pot_old_real)), aes(x=sd_Prob_Round16_old, y=sd_Prob_Round16_new)) +
  geom_label_repel(aes(label=Club), size=3, max.overlaps=100) + 
  geom_point(aes(color=as.factor(Pot_old)), size=2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray20") +
  labs(
    x = "SD of reaching Round of 16 (old format)",
    y = "SD of reaching Round of 16 (new format)",
    color = "Old format Pot",
    title = "Variance of the possible draw outcomes",
  ) + scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position="bottom")

#ggsave(file="draw_effect/Probs_oldvsnew_variances_noplayoff_perfectseeding_potequalized.pdf", scale=1.2)


#decompose effect
probs_all <- bind_rows(official, perf, noplayoff) |> 
  mutate(sd_diff=sd_Prob_Round16_new-sd_Prob_Round16_old) |> 
  select(-Pot_new, -Pot_old)

write_xlsx(probs_all, paste0("draw_effect/output/fig_data/Figures1_4_data_ci_", sampling, s, ".xlsx"))

probs_all_wide <- probs_all |> pivot_wider(names_from=scenario, values_from=c(sd_diff, Pot_old_real), id_cols=c(Club, Elo)) |> 
  mutate(noplayoff=sd_diff_noplayoff, 
         perfectseeding=sd_diff_perfectseeding-noplayoff,
    official=sd_diff_official-perfectseeding) |> filter(!is.na(sd_diff_official))
 
probs_all_long<- probs_all_wide |> pivot_longer(cols=noplayoff:official, names_to="scenario")

ggplot(data=probs_all_long, aes(x=value, y=fct_reorder(Club, Elo), fill=fct_reorder(scenario,value))) +
  geom_col() +
  scale_fill_manual(values = c("red4", "orange", "deepskyblue4"),
                    labels= c("seeding effect", "playoff effect", "first stage effect")) +
  xlab("difference in the SD of advancing probabilities between the new - old format") +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  theme_minimal() +
  theme(legend.title=element_blank(),
        axis.title.y=element_blank(),
        )

#ggsave(file=paste0("draw_effect/output/figures/decomposition3_", s, ".pdf"), scale=1.7)

#write_xlsx(probs_all_wide |> select(Club, Elo, format=noplayoff, playoff=perfectseeding, seeding=official), paste0("draw_effect/output/fig_data/Figure5_data", s, ".xlsx"))

#figure 6
probs_f6 <-probs_all |> pivot_longer(cols=c(sd_Prob_Round16_new, sd_Prob_Round16_old), names_to="format") |> 
  mutate(scenario=recode(scenario, "official"="UEFA coefficient", "perfectseeding"="Elo rating", "noplayoff"= "Elo rating, without playoff"),
         design=paste(str_sub(format, -3), scenario)) |> 
  filter(design != "old Elo rating, without playoff",
         !Club %in% c(probs_all |> filter(is.na(mean_Prob_Round16_old)) |> pull(Club) |> unique()))

probs_f6_lower <-probs_all |> pivot_longer(cols=c(lower_95_sd_new, lower_95_sd_old), names_to="format") |> 
  mutate(scenario=recode(scenario, "official"="UEFA coefficient", "perfectseeding"="Elo rating", "noplayoff"= "Elo rating, without playoff"),
         design=paste(str_sub(format, -3), scenario)) |> 
  filter(design != "old Elo rating, without playoff",
         !Club %in% c(probs_all |> filter(is.na(mean_Prob_Round16_old)) |> pull(Club) |> unique()))

probs_f6_upper <-probs_all |> pivot_longer(cols=c(upper_95_sd_new, upper_95_sd_old), names_to="format") |> 
  mutate(scenario=recode(scenario, "official"="UEFA coefficient", "perfectseeding"="Elo rating", "noplayoff"= "Elo rating, without playoff"),
         design=paste(str_sub(format, -3), scenario)) |> 
  filter(design != "old Elo rating, without playoff",
         !Club %in% c(probs_all |> filter(is.na(mean_Prob_Round16_old)) |> pull(Club) |> unique()))

probs_f6$lower <- probs_f6_lower$value
probs_f6$upper <- probs_f6_upper$value


ggplot(probs_f6,
       aes(x = Elo,
           y = value,
           color = design)) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = 0) +
  geom_point() +
  xlab("Elo rating") +
  ylab("Standard deviation") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

#ggsave(file=paste0("draw_effect/output/figures/figure6_ci", s, sampling, ".pdf"), scale=2)

#robustness of sd to the nr of simulations

probs_comp_10000 <- readRDS("draw_effect/data/comparison/robustness/repeated10x1000new_old.rds") |> select(-lower_95_sd, -upper_95_sd) |> mutate(simnr=rep(1:1, each=68), sims_permatch=10000)
probs_comp_1000 <- readRDS("draw_effect/data/comparison/robustness/repeated10.rds") |> select(-lower_95_sd, -upper_95_sd) |> mutate(simnr=rep(1:10, each=68), sims_permatch=1000)
probs_comp_100 <- readRDS("draw_effect/data/comparison/robustness/repeated10x100.rds") |> select(-lower_95_sd, -upper_95_sd) |> mutate(simnr=rep(1:10, each=68),sims_permatch=100)
probs_comp_comb <- bind_rows(
  probs_comp_10000 |> pivot_wider(names_from=format, values_from=sd_Prob_Round16, id_cols=c("Club", "Elo","sims_permatch", "simnr")),
  probs_comp_1000 |> pivot_wider(names_from=format, values_from=sd_Prob_Round16, id_cols=c("Club", "Elo","sims_permatch", "simnr")),
  probs_comp_100 |> pivot_wider(names_from=format, values_from=sd_Prob_Round16, id_cols=c("Club","Elo","sims_permatch", "simnr"))
)

probs_all <-bind_rows(official) |> 
  mutate(sd_diff=sd_Prob_Round16_new-sd_Prob_Round16_old) |> 
  select(-Pot_new, -Pot_old)
probs_comp_1000 <- probs_all |> pivot_longer(cols=c(sd_Prob_Round16_new, sd_Prob_Round16_old), names_to="format") |> 
  mutate(scenario=recode(scenario, "official"="UEFA coefficient", "perfectseeding"="Elo rating", "noplayoff"= "Elo rating, without playoff"),
         design=paste(str_sub(format, -3), scenario),
         format=paste(str_sub(format, -3)),
         sd_Prob_Round16=value) |> 
  filter(design != "old Elo rating, without playoff")


ggplot( mapping=aes(x = Elo,
           y = sd_Prob_Round16,
           shape = as.factor(format)
           )
       ) +
  # geom_errorbar(aes(ymin = lower,
  #                   ymax = upper),
  #               width = 0) +
  geom_point(data=probs_comp_100, size=1, color="green3") +
  geom_point(data=probs_comp_1000, size=1, color="blue") +
  geom_point(data=probs_comp_10000, size=1, color="red") +
  xlab("Elo rating") +
  ylab("Standard deviation") +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.title = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

# ggsave(file=paste0("draw_effect/output/figures/figure_newformat_simsrobustness_old_new", s, sampling, ".pdf"), scale=2)
# 
# write_xlsx(probs_comp_comb, "draw_effect/output/fig_data/robustness/draw_effect_simulations_sensitivity.xlsx")
# write_xlsx(probs_comp_all, "draw_effect/output/fig_data/robustness/draw_effect_newformat_1000x10000.xlsx")

