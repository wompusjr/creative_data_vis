#load the required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(chisq.posthoc.test)
library(ggcorrplot)
#getting the data from the adjusted database
data <- read_excel("data/cienega_sites.xlsx", guess_max = 10000)
###Site Total ----------
dfSites <- data.frame(NACRE=data$NACRE,NO_NACRE=data$"NON-NACRE")
dfSites <- na.omit(dfSites)
dfSites <- dfSites[-c(1,2,3,4,6,7,8,12,13),] #getting rid of the san pedro and non-tuscon basin sites
summary(dfSites)
#running the test
(chisqSites <- chisq.test(dfSites, correct = F))
#observed chi-square is 1355.6 w/ 17 degrees of freedom and a p-value <2.2e-16.
#this allows us to reject our null hypothesis
###Testing Residuals----------
chisqSites$observed
round(chisqSites$expected,2)
round(chisqSites$residuals, 2)
chisq.posthoc.test(dfSites, method = "bonferroni")
#statistically relevant sites
##Los Pozos (0.0000000)
##Valley Farms (0.00124)
##Wetlands (0.0000000)

#visualizing residuals
corrplot(chisqSites$residuals,is.cor = FALSE)
#which sites favor which shell type
##Los Pozos favors NO_NACRE (-3.82/2.23)
##Valley Farms NACRE (3.42/-1.99)
##Wetlands favors NACRE (12.56/-7.33)

###visualizing with a cienega-wide test-------
dfSites2 <- data.frame(NACRE=data$NACRE,NO_NACRE=data$"NON-NACRE")
dfSites2 <- na.omit(dfSites2)
dfSites2 <- dfSites2[-c(1,2,3,4),] #getting rid of JUST the san pedro sites
(chisqSites2 <- chisq.test(dfSites2, correct = F))
round(chisqSites2$residuals, 2)
chisq.posthoc.test(dfSites2, method = "bonferroni")
#statistically relevant sites in the TB
##Clearwater (0.000000)
##Los Pozos (0.0000000)
##Santa Cruz Bend (0.000000)
##Stone Pipe(0.0023)
##Wetlands (0.0000000)
corrplot(chisqSites2$residuals,is.cor = FALSE)
#which sites favor which shell type
##Clearwater favors NO NACRE (-4.37/5.05)
##Los Pozos favors NO NACRE (-14.21/16.43)
##Santa Cruz Bend favors NO NACRE (-6.62/7.65)
##Stone Pipe favors NO NACRE (-2.56/2.96)
##Wetlands favors NACRE(4.80/-5.55)
