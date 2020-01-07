setwd("~/Desktop/VaAP2018/bowl")
dsoff <- read_csv("drivesuccoff.csv")
dsdef <- read_csv("drivesuccdef.csv")
logocsv <- read.csv("ncaa_teamlogos.csv")
ds_all <- left_join(dsoff, dsdef, by = c('TeamCode' = 'TeamCode'))

ds_all$ScoringPct_def <- 1- ds_all$ScoringPct_def

ds_df <- left_join(logocsv, ds_all, by = c('team_code' = 'TeamCode'))
ds_df

ds_df %>%
  ggplot(aes(x=ScoringPct_off, y=ScoringPct_def)) + 
  geom_image(aes(image = url), size = 0.05) + 
  labs(x = "Scoring Opportunities per Drive", 
       y = "(1 - Scoring Opportunities Allowed per Drive)", 
       caption = "@_alex_stern_ | Data from PFF", 
       title = "Scoring Opportunities - Field Goal Attempted or TD") +  
       # subtitle = "Scoring Opportunity: Field Goal Attempted or TD") + 
  theme_bw() + 
  theme(panel.border = element_blank(), 
        axis.line.x = element_line(), 
        axis.line.y = element_line()) +
  theme(axis.title = element_text(size=12), 
        axis.text = element_text(size=10), 
        plot.title = element_text(size=16), 
        plot.subtitle = element_text(size=14), 
        plot.caption = element_text(size=12))

ggsave('scoring_opp.png', dpi=1000)
