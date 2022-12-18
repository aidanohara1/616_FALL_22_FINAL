# Summer 2003, policy marketed to 2 villages using BASIXs best sales force 148 
#   farmers purchased the policy Paying 74000 Rs (1800$) for coverage of 600 
#   total acres of land 15% commission on sales, negligible revenue far 
#   outweighed by staff time, developing customer relations
# 
farmersPurchase2003 <- 148
totalPremium2003 <- 74000 
totalLand2003 <- 600
commisions2003 <- totalPremium2003*0.15

costOfStaffTime <- commisions2003 # AT LEAST 


# BASIX at large 60000 borrowers in 4000 villages in 30 districts
# 
# loan products typically had an annual percentage rate of 24% primarily 
# individual, sometimes grouped joint liabilities. average load size of 8800 Rs,
# 
# typically repaid in 11 to 24 months, monthly repayments matched to cash flow? 
#    crop loans mature after harvest small enterprise loans require 
#    regular month payments.
# 
totalVillages <- 4000
loanInterest <- .24
averageLoanSize <- 8800
repaymentMonths <- runif(1,min = 11,max = 24) #just random for fun


# BASIX primary source of operating income is interest on the above loans 
# SEE INCOME EXHIBIT 2 
# 9% cost of capitol 2% provisioning for future loan losses FY 2003 expected 
#   long term loan loss rate was ~ 2% typical repayment rates for micro finance 
#   institutions ranged between 97% and 99% BASIX has a lower repayment rate 
#   then typical. Because of drought
#   BASIX had an on-time repayment rate of 93.3%? Do we need to consider this factor?

costOfCapitol <- 0.09
provisioning <- 0.02
typicalLossRate <- 0.02
ontimerepaymentRate <- 0.933
repaymentRate <- runif(1,min = 0.97,max = 0.99)

# BASIX also sells life insurance, first to borrowers, bundled with the loan, 
#    60000 borrowers policy covered 1.5 times the amount of the initial loan 
#   and eventually even to non-borrowers By 2004 BASIX is using 250 customer 
#   service agents serving exclusively areas where commercial banks and micro 
#   finance corporations were not operating areas previously served by local 
#   moneylenders 10% interest per month or higher no competition for insurance 
#   sales for at least 5-10 years
# 
lifeInsurancePolicies <- 60000
coverage <- 1.5 * averageLoanSize
agents2004 <- 250
locallyAvailableLoanRate <- 0.1 # per MONTH




# A BASIX client agriculture as a primary source of income in 
#     Andhra Pradesh (AP) 
# a single household of ~5 members, a couple, a parent, and two children. 
# ~3 acres of land ownership whereupon they would grow groundnuts or castor seed 
# 3 acres of groundnut yielded 1 ton (1000 kg) of seeds, 
#     which could be sold for 25-35 Rs / kg. 
# middle men take substantial cut of profits households also have farm animals, 
#  grow food crops (maize or sorghum) expenses include farming inputs, 
#  seed and fertilizer, food, clothing, and fuel
# 

householdSize <- 5
landOwnership <- 3 #acres
groundNutYeild <- 1000 #kilogram
groundNutSale <-sum(runif(groundNutYeild, min = 25, max = 35)) #cheeky

# farmAnimalCost <- 
# foodCropsCost <- 
# seedCost <-
# fertilizerCost <- 
# foodCost <-
# clothingCost <- 
# fuelCost <-




# SEE CLIENTELE INCOME DISTRIBUTION IN EXHIBIT 3



# Farmers in AP and Tamil Nadu grow rice, groundnuts, sorghum, and more? 
# limited to no irrigation rainfall levels are an important determinant of 
# agricultural productivity The quality of monsoons is an actual determinant 
# of economic growth nationwide two growing cycles Jun- Nov and Dec- Apr 
# monsoon occurs during Jun- Nov
# 
# Farmers had to pay for seeds, fertilizer and labor long before profits. 
# Goods were purchased on credit so farmers are in debt most of the year.
# 
# HOUSE HOLD RISK MANAGEMENT Grim situation with perilous, 
#  costly circumstances, harsh saving conditions.
# 
# FINANCIAL RISK MANAGEMENT SERVICES Helping the house hold risk management 
# situation needed a credit solution. However a credit solution was not enough. 
# Provisions for insurance against livestock death and crop failure would go far.
# 
# Crop insurance classically sold by the national government was a conditioned 
#  bundling for borrowing. BASIX clients are ineligible.
# 
# National crop insurance was no premier service though. 
#   Unequally distributed benefits and slow payments.
# 
# Earliest crop insurance product by BASIX in 2000, 3 villages, 
#   farmers pooled risk, individuals would pay a premium annually. 
# A fraction of the funds would be kept in a village account. 
#  Balance accumulated in a "multi-village" account. 
#  Farmers would internally accept or reject claims. 
#  Losses would be drawn from the village account, 
#       and then the "mutli-village" account. 
#  BASIX was liable for claims exceeding the total premiums accumulated.
# 
# Administration costs were too high. 
#   Farmers were reluctant to pay the high premiums.
# 
# RAINFALL INSURANCE
# 
# Using a designated rainfall station near clients: 
#    Policy would pay cash if measured rainfall was particularly low. 
#    Clients would pay premiums.
# 
# A private insurance company was brought in with international connections. 
#  BASIX would act only as a sales agent, and earn commissions. 
#  Not be responsible for the payouts.
# 
# How should the rainfall index be designed?
#   
# What should the targets and payouts be?
#   
# What would be the policy's price?
# 
# Specific policies per crop? 
# 
# One-size-fits all?
# 
# Which would be easier to sell?
# 
# PRODUCT DESIGN AND MARKETING
# 
# Pilot program in only a few of the villages. Mahbudnagar district of AP.
# 
# 2003 Pilot policy would target just groundnuts and castor seed.
# 
# Food and Agricultural Organization suggested that groundnut required 
#   approximately 500-700 millimeters of water per year to produce good yield.
# 
# Growing period of May/June - mid October.
# 
# A MODEL FOR THE RELATIONSHIP BETWEEN CROP LOSS AND RAINFALL DEFICIT
# 
# blah blah blah
# 
# Weights are assigned to each part of the growing season to calculate rainfall index.
# 
# Insurance policy sets 653 mm target rainfall index over the growing period: 
#   May 11 to October 17, 2003. 
# 
# SEE EXHIBIT 5 for summary of 2003 policy
# 
# BASIX ignores other crop categories and focuses only on 
#    groundnuts and castor seed.
# 
# The policy only sold 150 policies. 
# 
# Fixed costs of development and administration and the cost of field agents' 
#       time far outweighed first year profits.
# 
# Agents took 3 hours to sell 600 Rs insurance policies,
# 
# Agents took 6 hours to sell make and service an average size loan.
# 
# Neglecting customer service agent travel time.
# 
# THE 2004 REVISED PRODUCT
# 
# Farmers were concerned about transparency.
# 
# SEE EXHIBIT 5 for revised policy
# Insurance Policy for Groundnut in 2014
period_1_normal <- 75
period_1_max <- 20
period_1_normal_payment <- 15
period_1_max_payment <- 3000

period_2_normal <- 110
period_2_max <- 10
period_2_normal_payment <- 40
period_2_max_payment <- 2000

period_3_normal <- 75
period_3_max <- 5
period_3_normal_payment <- 10
period_3_max_payment <- 1000

# SEE EXHIBIT 6 for historical distribution of 
#    rainfall for the periods covered by the policy.
n1 <- 35 
n2 <- 35
n3 <- 45

set.seed(1234) 
rlnorm2 <- function(n, mean, sd){
  rlnorm(n, log(mean*(1+sd^2/mean^2)^-.5), log(1+sd^2/mean^2)^.5)
}

phase_1_rainfall <- rlnorm2(n1, mean = 115, sd = 56)
phase_2_rainfall <- rlnorm2(n2, mean = 191.1, sd = 82.5)
phase_3_rainfall <- rlnorm2(n3, mean = 209.7, sd = 97.7)

hist(phase_1_rainfall)
hist(phase_2_rainfall)
hist(phase_3_rainfall)

# 
# DECISION
# 
# 2003 rainfall resulted in modest payouts. 
# 
# Farmers were satisfied about the speed of payouts. 
# 
# Good interest in policy for next monsoon season.
# 
# Caution was important though, trust was long earned.
# 
# Bundling the insurance and the loans could 
#      result in misunderstanding and default. 
# 
# Would the policy sell well?

#    What is the cost/benefit of the policy for all the farmers of different 
#           income levels?
#   What were the outcomes for farmers in the 2003 policy?

#   
#   What was the best way to explain it to clients?
#   
#   How to avoid "gouging' its captive clients?
# 
# Was the policy fair?
# 
# How could it help BASIX clients?

### WHAT WERE THE OTHER IMPORTANT 
### RISKS, 
### COSTS, or 
### BENEFITS that hadn't been considered?




