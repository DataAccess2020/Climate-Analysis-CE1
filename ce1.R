library(rio)
library(tidyverse)
setwd("/Users/catalinamasp/Documents/UNIMI/R")
ess <- import("ESS8e02.2_F1.sav")
ess_pg <- ess[, c("sbsrnen","cntry")]
ess_pg2 <- na.omit(ess_pg)
ess_pg2 <-ess_pg2 %>%
  mutate(recoded_opinion = ifelse(sbsrnen <= 2, "Favor",
                                  ifelse(sbsrnen == 3, "Neutral",
                                          "Against")))
ess_pg2 <- filter(
  ess_pg2,
  cntry %in% c("DE", "IT")
)
ess_pg2 %>% 
  group_by(recoded_opinion, cntry)%>% 
  count()%>% 
ggplot(aes(x = recoded_opinion, y = n, color = cntry, fill = cntry))+
       geom_col()+
  facet_wrap(~ cntry, scales = "free_y")
       theme_bw()  
       