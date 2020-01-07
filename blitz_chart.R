setwd("~/Desktop/VaAP2018/bowl")
blitzoff <- read_csv("blitzoff.csv")
blitzdef <- read_csv("blitzdef.csv")
logocsv <- read.csv("ncaa_teamlogos.csv")
blitz_all <- left_join(blitzoff, blitzdef, by = c('TeamCode' = 'TeamCode'))

blitz_all$SuccessRate_def <- 1- blitz_all$SuccessRate_def

blitz_df <- left_join(logocsv, blitz_all, by = c('team_code' = 'TeamCode'))
blitz_df

blitz_df %>%
  ggplot(aes(x=SuccessRate_off, y=SuccessRate_def)) + 
  geom_image(aes(image = url), size = 0.05) + 
  labs(x = "Offensive Success Rate", 
       y = "(1 - Offensive Success Rate Allowed)", 
       caption = "@_alex_stern_ | Data from PFF", 
       title = "When the Defense Blitzes") +  
  # subtitle = "2019 Season to Date") + 
  theme_bw() + 
  theme(panel.border = element_blank(), 
        axis.line.x = element_line(), 
        axis.line.y = element_line()) +
  theme(axis.title = element_text(size=12), 
        axis.text = element_text(size=10), 
        plot.title = element_text(size=16), 
        plot.subtitle = element_text(size=14), 
        plot.caption = element_text(size=12))

ggsave('blitz_chart.png', dpi=1000)
