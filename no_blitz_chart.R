setwd("~/Desktop/VaAP2018/bowl")
noblitzoff <- read_csv("noblitzoff.csv")
noblitzdef <- read_csv("noblitzdef.csv")
logocsv <- read.csv("ncaa_teamlogos.csv")
noblitz_all <- left_join(noblitzoff, noblitzdef, by = c('TeamCode' = 'TeamCode'))

noblitz_all$SuccessRate_def <- 1- noblitz_all$SuccessRate_def

noblitz_df <- left_join(logocsv, noblitz_all, by = c('team_code' = 'TeamCode'))
noblitz_df

noblitz_df %>%
  ggplot(aes(x=SuccessRate_off, y=SuccessRate_def)) + 
  geom_image(aes(image = url), size = 0.05) + 
  labs(x = "Offensive Success Rate", 
       y = "(1 - Offensive Success Rate Allowed)", 
       caption = "@_alex_stern_ | Data from PFF", 
       title = "When the Defense Doesn't Blitz") +  
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

ggsave('no_blitz_chart.png', dpi=1000)
