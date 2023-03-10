library(tidyverse)
library(extraDistr)


#Charecteristics of BASIX clients

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
secondInt <- (acreBreaks[3] - acreBreaks[2] + 1)/2 #+1 for better distr.


# landAcres <- ifelse(incomeCategory == 1, 
#                     rtriang(1, 
#                             a = acreBreaks[1] - firstInt,
#                             b = acreBreaks[2] + firstInt),
#                     ifelse(incomeCategory == 2,
#                            rtriang(1,
#                                    a = acreBreaks[2] - firstInt,
#                                    b = acreBreaks[2] + secondInt),
#                            rtriang(1,
#                                    a = acreBreaks[3] - secondInt,
#                                    b = acreBreaks[3] + secondInt)))

landAcres <- function(incomeCategory) {
  acres <- ifelse(incomeCategory == 1, 
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
  return(acres)
}



farmers <- data.frame(income = incomeCategory,
                      land = sapply(incomeCategory,landAcres))



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

hist(farmers$netPolicyResult, 
     breaks = 20, 
     xlim=c(-125,5000),
     main="2004 Policy Payout Simulation",
     xlab="Total Payout",
     col = "#009999")                                
abline(v = mean(farmers$netPolicyResult),                     
       col = "red",
       lwd = 1)
text(x = mean(farmers$netPolicyResult) * 10,                
     y = mean(farmers$netPolicyResult) * 10,
     paste("Mean =",round(mean(farmers$netPolicyResult), digit = 2)),
     col = "red",
     cex = 2)


farmers2003 <- farmers[,1:5]

# 3 periods of rainfall
# recall our historical distributions
# phase 1 rainfall is measured between Jun 10 - Jul 15
# phase 2 rainfall is measured between Jul 16 - Aug 20
# phase 3 rainfall is measured between Aug 21 - Oct 12
# in 2003 each period is as followers
# period 1: May 11 - Jun 10 <~ modified phase 1 rainfall
# period 2: Jun 10 - Jul 15 <~ phase 1 rainfall
# period 3: Jul 15 - Oct 17 <~ phase 2 and phase 3 rainfall

rainIndexWeights <- c(.25,3,.75)
premiums <- c(900,600,450)
notionalTarget <- 653

whichPolicy <- function(landSize) {
   policy <- ifelse(landSize < 2.5,3,
                    ifelse(landSize < 5,2,1))
   return(policy)
}

whichPremium <- function(policy) {
  premium <- ifelse(policy == 3, premiums[3],
                    ifelse(policy == 2, premiums[2],
                           premiums[1]))
  return(premium)
}

policies <- sapply(farmers2003$land, whichPolicy)
premiumCosts <- sapply(policies, whichPremium)

farmers2003 <- mutate(farmers2003,
                      policy = policies,
                      cost = -premiumCosts)

shortfallAbv75 <- c(30,25,20)
shortfallAbv35 <- c(175,100,75)
shortfallBel35 <- c(650,500,310)

phase_0_rainfall <- rlnorm2(n, mean = 90, sd = 50)

farmers2003 <- mutate(farmers2003,
                      period1 = phase_0_rainfall,
                      period2 = rain1,
                      period3 = rain2 + rain3,
                      weighted1 = period1 * rainIndexWeights[1],
                      weighted2 = period2 * rainIndexWeights[2],
                      weighted3 = period3 * rainIndexWeights[3])

totalRainIndex <- select(farmers2003,weighted1,weighted2,weighted3)
totalIndex <- rowSums(totalRainIndex)
farmers2003 <- mutate(farmers2003, 
                  totalRainIndexes = totalIndex,
                  targetDiff = totalRainIndexes - notionalTarget,
                  perRain = 1 + targetDiff/notionalTarget)




payouts2003 <- function(policy, perRainfall) {
  paymentsPerShort <- c(shortfallAbv75[policy],
                        shortfallAbv35[policy],
                        shortfallBel35[policy])
  if(perRainfall >= 0.95) {
    payout = 0
  }
  if(perRainfall < 0.95 && perRainfall > 0.75) {
         payout = ((0.95 - perRainfall)*100) * paymentsPerShort[1]
  } 
  if(perRainfall <= 0.75 && perRainfall >= 0.35) {
    payout = ((0.75 - perRainfall)*100) * paymentsPerShort[2] +
      ((0.95 - 0.75)*100) * paymentsPerShort[1]
  }
  if(perRainfall < 0.35) {
    payout = ((0.35 - perRainfall)*100) * paymentsPerShort[3] +
      ((0.75 - 0.35)*100) * paymentsPerShort[2] +
      ((0.95 - 0.75)*100) * paymentsPerShort[1]
  }
  return(payout)
}

farmerPayouts <- mapply(payouts2003, policy = farmers2003$policy,
                        perRainfall = farmers2003$perRain)
farmers2003 <- mutate(farmers2003, policyPayout = farmerPayouts,
                      netPolicyResult = cost + policyPayout)


hist(farmers2003$netPolicyResult, 
     breaks = 40, 
     xlim= c(-125,5000),
     ylim = c(0,0.0004),
     main="2003 Policy Payout Simulation",
     xlab="Total Payout",
     col = "#00CC00",
     freq = F)                                
abline(v = mean(farmers2003$netPolicyResult),                     
       col = "red",
       lwd = 30)
text(x = 1000,                
     y = 0.0003,
     paste("Mean =",round(mean(farmers2003$netPolicyResult), digit = 2)),
     col = "red",
     cex = 2)

##### BASIX model

policiesSold <- seq(100,350,10)

policyHolders <- function(m) {
  n = m
  random <- runif(n)
  incomeCategory <- ifelse(random < .21, 1, 
                           ifelse(random < (.21 + .16),2,3))
  holders <- data.frame(income = incomeCategory,
                        land = sapply(incomeCategory,landAcres))
  policies <- sapply(holders$land, whichPolicy)
  premiumCosts <- sapply(policies, whichPremium)
  holders <- mutate(holders,
                    policy = policies,
                    cost_03 = -premiumCosts,
                    cost_04 = -premium2004)
}

rainSims <- function(p) {
  n = p
  phase_0_rainfall <- rlnorm2(n, mean = 90, sd = 50)
  phase_1_rainfall <- rlnorm2(n, mean = 115, sd = 56)
  phase_2_rainfall <- rlnorm2(n, mean = 191.1, sd = 82.5)
  phase_3_rainfall <- rlnorm2(n, mean = 209.7, sd = 97.7)
  return(data.frame(rain0 = phase_0_rainfall,
                    rain1 = phase_1_rainfall,
                    rain2 = phase_2_rainfall,
                    rain3 = phase_3_rainfall
  ))
}

holdersGetRain <- function(rainVector, listOfHolders) {
  rainVector <- as.numeric(rainVector)
  holds <- mutate(listOfHolders, rain0 = rainVector[1],
                    rain1 = rainVector[2], 
                    rain2 = rainVector[3],
                    rain3 = rainVector[4])

  #2004
  holds <- mutate(holds, diff1 = rain1 - rainIndex[1],
                    diff2 = rain2 - rainIndex[2],
                    diff3 = rain3 - rainIndex[3],
                    payout1 = phasePayout(diff1,1),
                    payout2 = phasePayout(diff2,2),
                    payout3 = phasePayout(diff3,3))
  payouts <- select(holds,payout1,payout2,payout3)
  totalPayouts <- rowSums(payouts)
  holds <- mutate(holds, 
                    totalPayout_04 = totalPayouts,
                    netPolicyResult_04 = totalPayout_04 + cost_04)
  
  #2003
  
  holds <- mutate(holds,
                        period1 = rain0,
                        period2 = rain1,
                        period3 = rain2 + rain3,
                        weighted1 = period1 * rainIndexWeights[1],
                        weighted2 = period2 * rainIndexWeights[2],
                        weighted3 = period3 * rainIndexWeights[3])
  totalRainIndex <- select(holds,weighted1,weighted2,weighted3)
  totalIndex <- rowSums(totalRainIndex)
  holds <- mutate(holds, 
                        totalRainIndexes_03 = totalIndex,
                        targetDiff_03 = totalRainIndexes_03 - notionalTarget,
                        perRain_03 = 1 + targetDiff_03/notionalTarget)
  
  payouts_03 <- mapply(payouts2003, policy = holds$policy,
                          perRainfall = holds$perRain_03)
  holds <- mutate(holds, totalPayout_03 = payouts_03,
                        netPolicyResult_03 = cost_03 + totalPayout_03)
  return(c(sum(holds$netPolicyResult_03),sum(holds$netPolicyResult_04)))
}


rainSimulation <- rainSims(1000)
runRainSims <- function(listOfHolders) {
  npv_03 <- NULL
  npv_04 <- NULL
  for (i in 1:dim(rainSimulation)[1]) {
    npvList <- as.numeric(holdersGetRain(rainVector = rainSimulation[i,],
                              listOfHolders = listOfHolders))
    npv_03[i] <- npvList[1]
    npv_04[i] <- npvList[2]
  }
  return(data.frame(npv_03,npv_04))
}
##7 minutes##
mean_03 <- NULL
mean_04 <- NULL
policyNumBreaks <- for(i in 1:length(policiesSold)) {
  simulatedNPVs <- runRainSims(policyHolders(policiesSold[i]))
  mean_03[i] <- mean(simulatedNPVs$npv_03)
  mean_04[i] <- mean(simulatedNPVs$npv_04)
}


theBigModel <- data.frame(policiesSold,
                          -mean_03,
                          -mean_04)


# 03 means are profitable for BASIX because of higher premiums and lower
#   likelihood of larger payout.
# 04 means are profitable for the farmers because of the widespread effect of 
#  falling below a max payout trigger.
