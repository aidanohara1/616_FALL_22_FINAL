library(tidyverse)
library(extraDistr)


#Charecteristics of BASIX clients

clientIncomeBreaks <- c(2000,3000)
clientIncomeRangePer <- c(.21,.16,.60)

clientIncome <- read.csv("incomeDistributions.csv")
clientHousehold <- read.csv("householdCharecteristics.csv")

clientHousehold2 <- t(clientHousehold)
colnames(clientHousehold2) = clientHousehold2[1,]
clientHousehold2 <- clientHousehold2[2:4,]

# a farmer of some income level,
# with some amount of landownership, 


# buys a policy, 

### CROPS grow and rain falls

# Dependent on how much rain falls during each part of the season
#  An estimated crop yield is calculable
#  and an estimated policy payout is determined



### FARMER model

# each farmer is of one designated monthly income category
n = 10000
random <- runif(n)
incomeCategory <- ifelse(random < .21, 1, 
                         ifelse(random < (.21 + .16),2,3))

# each farmer has some amount of land, corresponding to their income category
acreBreaks <- as.numeric(clientHousehold2[,11])
firstInt <- (acreBreaks[2] - acreBreaks[1])/2
secondInt <- (acreBreaks[3] - acreBreaks[2])/2


landAcres <- ifelse(incomeCategory == 1, 
                    rtriang(1, 
                            a = acreBreaks[1] - firstInt,
                            b = acreBreaks[2] + firstInt),
                    ifelse(incomeCategory == 2,
                           rtriang(1,
                                   a = acreBreaks[2] - firstInt,
                                   b = acreBreaks[2] + secondInt),
                           rtriang(1,
                                   a = acreBreaks[3] - secondInt,
                                   b = acreBreaks[3] + secondInt)))



farmers <- data.frame(income = incomeCategory,
                      land = landAcres)



### From group

set.seed(1234) 
rlnorm2 <- function(n, mean, sd){
  rlnorm(n, log(mean*(1+sd^2/mean^2)^-.5), log(1+sd^2/mean^2)^.5)
}

phase_1_rainfall <- rlnorm2(n, mean = 115, sd = 56)
phase_2_rainfall <- rlnorm2(n, mean = 191.1, sd = 82.5)
phase_3_rainfall <- rlnorm2(n, mean = 209.7, sd = 97.7)

farmers <- mutate(farmers, rain1 = phase_1_rainfall, 
                 rain2 = phase_2_rainfall,
                 rain3 = phase_3_rainfall)

### Crop yield from rainfall (later)
# phase 1 rainfall is measured between Jun 10 - Jul 15
# phase 2 rainfall is measured between Jul 16 - Aug 20
# phase 3 rainfall is measured between Aug 21 - Oct 12


### Policy Payout from rainfall
premium2004 <- 125

rainIndex <- c(75,110,75)
shortfallPayments <- c(15,10,5)
maxTriggers <- c(-20,-40,-10)
maxPayouts <- c(3000,2000,1000)


### 2004 
farmers <- mutate(farmers, premium = -premium2004,
                  diff1 = rain1 - rainIndex[1],
                  diff2 = rain2 - rainIndex[2],
                  diff3 = rain3 - rainIndex[3])

phasePayout <- function(difference, phase) {
  payout <- ifelse(difference < maxTriggers[phase], 
         maxPayouts[phase],
         ifelse(difference < 0,
                -difference*shortfallPayments[phase],
                0))
  return(payout)
}

farmers <- mutate(farmers, 
                  payout1 = phasePayout(diff1,1),
                  payout2 = phasePayout(diff2,2),
                  payout3 = phasePayout(diff3,3))

payouts <- select(farmers,payout1,payout2,payout3)
totalPayouts <- rowSums(payouts)
farmers <- mutate(farmers, 
                  totalPayout = totalPayouts,
                  netPolicyResult = totalPayout + premium)

mean(farmers$netPolicyResult)
hist(farmers$netPolicyResult)

