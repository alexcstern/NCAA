# ncaa run sonars
library(tidyverse)
library(ggplot2)
library(viridis)
setwd("~/Desktop/VaAP2018/bowl")
input <- read_csv("bowl.csv")
runs <- input %>%
  filter(pff_RUNPASS == "R", pff_POAACTUAL == "LE" | 
           pff_POAACTUAL == "LG" | pff_POAACTUAL == "LT" | 
           pff_POAACTUAL == "ML" | pff_POAACTUAL == "MR" | 
           pff_POAACTUAL == "RG" | pff_POAACTUAL == "RT" | 
           pff_POAACTUAL == "RE")
first <- read_csv("pl4.csv")
allp <- read_csv("player_list.csv")
first_players <- first$player
second_players <- second$player
all_players <- allp$player
player_lookup <- function(player) {
  name <- ""
  for (i in 1:40) {
    if (all_players[i] == player) {
      name <- allp$name[i]
    }
  }
  name
}
player_lookup("OKTU 03")
runs <- runs %>% filter(pff_BALLCARRIER %in% first$player)
summary(pl_names)
pl_names <- c()
for (i in 1:nrow(runs)) {
  pl <- runs[i,"pff_BALLCARRIER"]
  pll <- player_lookup(pl)
  if (pll == "") {
    print('NA')
  }
  pl_names <- c(pl_names, pll)
}
runs$names <- pl_names
player_counts <- runs %>% count(pff_BALLCARRIER)
player_counts <- player_counts[order(-player_counts$n),]
top_runners <- head(player_counts$pff_BALLCARRIER, 50)

# 97 - ML, 112 - LG, 127 - LT, 142 - LE
# 83 - MR, 68 - RG, 53 - RT, 38 - RE
90 - 7 - 45

runs <- runs %>% 
  filter(pff_BALLCARRIER %in% top_runners) %>%
  mutate(ang = ifelse(pff_POAACTUAL == "LE", 38, 
                      ifelse(pff_POAACTUAL == "LT", 53, 
                             ifelse(pff_POAACTUAL == "LG", 68, 
                                    ifelse(pff_POAACTUAL == "ML", 83, 
                                           ifelse(pff_POAACTUAL == "MR", 97, 
                                                  ifelse(pff_POAACTUAL=="RG", 112,
                                                         ifelse(pff_POAACTUAL=="RT",127,142))))))))
runs <- runs %>%
  mutate(succ_rate = ifelse(pff_DOWN == 1, ifelse(pff_GAINLOSS >= .4*pff_DISTANCE, 1, 0), 
                            ifelse(pff_DOWN == 2, ifelse(pff_GAINLOSS >= .5*pff_DISTANCE, 1, 0), 
                                   ifelse(pff_GAINLOSS >= pff_DISTANCE, 1, 0))))

round_angle <- 15
runs <- runs %>%
  mutate(AngleRound = round(ang/round_angle)*round_angle)
sonar <- runs %>%
  mutate(N=n()) %>%
  group_by(AngleRound, names) %>%
  mutate(n_rushes = n(), n_angle=n_rushes/N) %>%
  ungroup() %>%
  group_by(names) %>%
  mutate(maxN = max(n_angle), 
         AngleNorm = n_angle/maxN) %>%
  ungroup() %>%
  group_by(AngleRound, names, N) %>%
  summarize(AngleNorm = mean(AngleNorm), 
            SuccessRate = mean(succ_rate)) %>%
  arrange(names)

options(repr.plot.width=12, repr.plot.height=8)
plot_sonars <- ggplot(sonar) + 
  geom_bar(aes(x=AngleRound, y=AngleNorm, fill=SuccessRate), 
           stat='identity') + 
  scale_x_continuous(breaks=seq(0,360,by=90), limits=c(0,360)) + 
  coord_polar(start=4.58, direction = 1) + 
  scale_fill_viridis("Success %", na.value="#FDE725FF",
                     limits=c(0.3,0.7),
                     breaks=c(0.3,0.4,0.5,0.6,0.7),
                     labels = scales::percent) + 
  labs(x='', y='') + 
  theme_void(15) + 
  theme(plot.title = element_text(size=14), #hjust-0.5, size=16),
        legend.title = element_text(size=12), #hjust=1, size=12),
        # axis.title = element_text(size=12), 
        # axis.text = element_text(size=10),
        plot.subtitle = element_text(size=12), 
        plot.caption = element_text(size=10),
        strip.text.x = element_text(size = 7.5),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA),
        panel.spacing = unit(0, "lines"),
        plot.margin = margin(0,0,0,0,"cm")) + 
  facet_wrap(~names, nrow=5) + 
  labs(title = "Player Sonars by POA, FBS 2019", 
       subtitle = "Top 40 in Rushing Attempts (4/4)", 
       caption = "@_alex_stern_ | Data from PFF")



plot_sonars
ggsave("player_sonar4.png", dpi=1000)









