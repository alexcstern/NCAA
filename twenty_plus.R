setwd("~/Desktop/VaAP2018/bowl")
twoff <- read_csv("twentyoff.csv")
twdef <- read_csv("twentydef.csv")
logocsv <- read.csv("ncaa_teamlogos.csv")
tw_all <- left_join(twoff, twdef, by = c('TeamCode' = 'TeamCode'))

tw_all$SuccessRate_def <- 1- tw_all$SuccessRate_def

tw_df <- left_join(logocsv, tw_all, by = c('team_code' = 'TeamCode'))
tw_df

tw_df %>%
  ggplot(aes(x=SuccessRate_off, y=SuccessRate_def)) + 
  geom_image(aes(image = url), size = 0.05) + 
  labs(x = "Offensive Success Rate", 
       y = "(1 - Offensive Success Rate Allowed)", 
       caption = "@_alex_stern_ | Data from PFF", 
       title = "20+ Yard Passes") +  
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

ggsave('twenty_plus_chart.png', dpi=1000)
