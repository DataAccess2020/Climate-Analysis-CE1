library(rio)
library(tidyverse)
setwd("/Users/catalinamasp/Documents/UNIMI/R")
ess <- import("ESS8e02.2_F1.sav")
ess_pg <- ess[, c("sbsrnen","cntry")]
ess_pg2 <- na.omit(ess_pg)

#Rename variable 

  names(ess_pg2)[names(ess_pg2) == "sbsrnen"] <- "SubsidiseRenewableEnergy"
  
  #Recode variable into three groups

ess_pg2 <-ess_pg2 %>%
  mutate(recoded_opinion = ifelse(SubsidiseRenewableEnergy <= 2, "Favor",
                                  ifelse(SubsidiseRenewableEnergy == 3, "Neutral",
                                          "Against")))

#Recode countries into regions 

ess_pg2 <- ess_pg2 %>%
  mutate(region = ifelse(cntry == "ES" | cntry == "PT" | cntry =="IT", "Southern Countries", 
  ifelse(cntry == "IS" | cntry == "SE" | cntry == "FI" | cntry == "NO", "Northern Countries", 
  "Others" )))

ess_pg2 <- filter(
  ess_pg2,
  region %in% c("Southern Countries", "Northern Countries")
)

#Plotting frequency distribution

ess_pg2 %>%
  group_by(recoded_opinion, region, cntry)%>% 
  count()%>% 
ggplot(aes(x = recoded_opinion, y = n, color = region, fill = region))+
       geom_col()+
  facet_wrap(~ region)
       theme_bw()  

# Original Dataset for including an independent variable and see the relationship 
       
ess <- ess%>%
    mutate(region = ifelse(cntry == "ES" | cntry == "PT" | cntry =="IT", "Southern Countries", 
                      ifelse(cntry == "IS" | cntry == "SE" | cntry == "FI" | cntry == "NO", "Northern Countries", 
                                "Others" )))

ess_pg3 <- ess[, c("sbsrnen","region","wrclmch")]
ess_pg3 <- na.omit(ess_pg3)
names(ess_pg3)[names(ess_pg3) == "sbsrnen"] <- "SubsidiseRenewableEnergy"
names(ess_pg3)[names(ess_pg3) == "wrclmch"] <- "WorriedClimateChange"

ess_pg3 <- filter(
    ess_pg3,
    region %in% c("Southern Countries", "Northern Countries")
) 

#Plotting the relationship 

ggplot(ess_pg3, (aes(x = SubsidiseRenewableEnergy, y = WorriedClimateChange))) +
    geom_smooth(method = "lm") +
    theme_bw()

#Linear Regression

L.Regression <- lm(SubsidiseRenewableEnergy ~ WorriedClimateChange,
      data = ess_pg3)
summary(L.Regression)