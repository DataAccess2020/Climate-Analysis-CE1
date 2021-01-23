library(rio)
library(tidyverse)
setwd("D:/COM/Data analysis/ESS8e02_1.stata")
ess <- import("ESS8e02_1.dta")
ess_pg <- ess[, c("sbsrnen","cntry")]
ess_pg2 <- na.omit(ess_pg)
ess_pg2 <-ess_pg2 %>%
  mutate(recoded_opinion = ifelse(sbsrnen <= 2, "Favor",
                                  ifelse(sbsrnen == 3, "Neutral",
                                          "Against")))
ess_pg2 <- ess_pg2 %>%
  mutate(region = ifelse(cntry == "ES" | cntry == "PT" | cntry =="IT", "Southern Countries", 
  ifelse(cntry == "IS" | cntry == "SE" | cntry == "FI" | cntry == "NO", "Northern Countries", 
  "Others" )))

ess_pg2 <- filter(
  ess_pg2,
  region %in% c("Southern Countries", "Northern Countries")
)

ess_pg2 %>%
  group_by(recoded_opinion, region, cntry)%>% 
  count()%>% 
ggplot(aes(x = recoded_opinion, y = n, color = region, fill = region))+
       geom_col()+
  facet_wrap(~ region)
       theme_bw()  
