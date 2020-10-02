
options ("scipen"=10000, "digits"= 6)
set.seed(8000)



#We load the data file

countries <- readRDS(file = "countries_3.rds")





#Create a DF for all europe
europe <- bind_rows(countries) #and create europe again



# Ineq functions

#gini
wginif <- function(df, var, wg) {
  gini <- gini.wtd (df[[var]], df[[wg]])
  gini
}

#theil
wmldf <- function(df, var, wg) {
  mld <- mld.wtd (df[[var]], df[[wg]])
  mld
}


#mean general function
wmeanf <- function(df, var, wg) {
  mean <- weighted.mean(df[[var]], df[[wg]], na.rm = T)
  mean
}


## WE DO THE DECOMPOSITION IN A BOOTSTRAP TO GET THE SD OF THE ESTIMATIONS ####

#create empty vectors to fill in the bootstrap
btlockblboot <- vector()
wthlockblboot <- vector()
eurmldlockblboot <- vector()

btlock2lboot <- vector()
wthlock2lboot <- vector()
eurmldlock2lboot <- vector()

btlock2l6c20boot <- vector()
wthlock2l6c20boot <- vector()
eurmldlock2l6c20boot <- vector()

btlock2l6c30boot <- vector()
wthlock2l6c30boot <- vector()
eurmldlock2l6c30boot <- vector()

btlock2l6c40boot <- vector()
wthlock2l6c40boot <- vector()
eurmldlock2l6c40boot <- vector()

sharebtlockblboot<- vector()
sharewthlockblboot <- vector()

sharebtlock2l6c20boot<- vector()
sharewthlock2l6c20boot <- vector()

sharebtlock2l6c30boot<- vector()
sharewthlock2l6c30boot <- vector()

sharebtlock2l6c40boot<- vector()
sharewthlock2l6c40boot <- vector()

sharebtlock2lboot <- vector()
sharewthlock2lboot <- vector()

incabs2leurboot <- vector()
incabs2l6c20eurboot <- vector()
incabs2l6c30eurboot <- vector()
incabs2l6c40eurboot <- vector()


incabs2lbtboot <- vector()
incabs2l6c20btboot <- vector()
incabs2l6c30btboot <- vector()
incabs2l6c40btboot <- vector()


incabs2lwthboot <- vector()
incabs2l6c20wthboot <- vector()
incabs2l6c30wthboot <- vector()
incabs2l6c40wthboot <- vector()


inc2leurboot <- vector()
inc2l6c20eurboot <- vector()
inc2l6c30eurboot <- vector()
inc2l6c40eurboot <- vector()


inc2lbtboot <- vector()
inc2l6c20btboot <- vector()
inc2l6c30btboot <- vector()
inc2l6c40btboot <- vector()


inc2lwthboot <- vector()
inc2l6c20wthboot <- vector()
inc2l6c30wthboot <- vector()
inc2l6c40wthboot <- vector()


for (n in 1:100)
{countriesbt <- lapply (countries, function(df) df [sample(nrow(df), replace=TRUE),]) #bootstrap resampling

europe <- bind_rows(countriesbt) #create data of overall Europe (all countries)

weighteurope <- sum(europe[["weight"]])
wshare <- sapply (countriesbt, function(X)  { sum(X[["weight"]])/weighteurope}) #weight of each country

mldlockbl <- sapply (countriesbt, function(X) wmldf (df = X, "wage", "weight")) #wage inequality by country

meanwagelockbl <- sapply (countriesbt, function(X) wmeanf (df = X, "wage", "weight")) #mean wage by country

#between inequality
btlockbl <- mld.wtd(meanwagelockbl, weights = wshare)
#within inequality
wthlockbl <- weighted.mean(mldlockbl, w = wshare)

#get bootrsapped sd of the estimates (also the mean, although we won't use that)

btlockblboot[n] <- btlockbl
wthlockblboot[n] <- wthlockbl
btlockblbootmean <- mean (btlockblboot)
btlockblbootsd <- sd (btlockblboot)
wthlockblbootmean <- mean (wthlockblboot)
wthlockblbootsd <- sd (wthlockblboot)

eurmldlockbl <- mld.wtd(europe$wage, europe$weight)
eurmldlockblboot[n] <- eurmldlockbl
eurmldlockblbootmean <- mean (eurmldlockblboot)
eurmldlockblbootsd <- sd (eurmldlockblboot)

sharebtlockblboot [n] <-  (btlockbl/eurmldlockbl)*100
sharewthlockblboot [n] <-  (wthlockbl/eurmldlockbl)*100

sharebtlockblbootmean <-  mean (sharebtlockblboot)
sharebtlockblbootsd <-  sd (sharebtlockblboot)

sharewthlockblbootmean <-  mean (sharewthlockblboot)
sharewthlockblbootsd <-  sd (sharewthlockblboot)



### now with the 2l loss

mldlock2l <- sapply (countriesbt, function(X) wmldf (df = X, "wage2l", "weight"))

meanwagelock2l <- sapply (countriesbt, function(X) wmeanf (df = X, "wage2l", "weight"))

#between inequality
btlock2l <- mld.wtd(meanwagelock2l, weights = wshare)
wthlock2l <- weighted.mean(mldlock2l, w = wshare)

btlock2lboot[n] <- btlock2l
wthlock2lboot[n] <- wthlock2l
btlock2lbootmean <- mean (btlock2lboot)
btlock2lbootsd <- sd (btlock2lboot)
wthlock2lbootmean <- mean (wthlock2lboot)
wthlock2lbootsd <- sd (wthlock2lboot)

eurmldlock2l <- mld.wtd(europe$wage2l, europe$weight)
eurmldlock2lboot[n] <- eurmldlock2l
eurmldlock2lbootmean <- mean (eurmldlock2lboot)
eurmldlock2lbootsd <- sd (eurmldlock2lboot)

sharebtlock2lboot [n] <-  (btlock2l/eurmldlock2l)*100
sharewthlock2lboot [n] <-  (wthlock2l/eurmldlock2l)*100

sharebtlock2lbootmean <-  mean (sharebtlock2lboot)
sharebtlock2lbootsd <-  sd (sharebtlock2lboot)

sharewthlock2lbootmean <-  mean (sharewthlock2lboot)
sharewthlock2lbootsd <-  sd (sharewthlock2lboot)


### Now with the 2l loss and 6m closure 20 ####

mldlock2l6c20 <- sapply (countriesbt, function(X) wmldf (df = X, "wage2l6c20", "weight"))

meanwagelock2l6c20 <- sapply (countriesbt, function(X) wmeanf (df = X, "wage2l6c20", "weight"))

#between inequality
btlock2l6c20 <- mld.wtd(meanwagelock2l6c20, weights = wshare)
wthlock2l6c20 <- weighted.mean(mldlock2l6c20, w = wshare)

btlock2l6c20boot[n] <- btlock2l6c20
wthlock2l6c20boot[n] <- wthlock2l6c20
btlock2l6c20bootmean <- mean (btlock2l6c20boot)
btlock2l6c20bootsd <- sd (btlock2l6c20boot)
wthlock2l6c20bootmean <- mean (wthlock2l6c20boot)
wthlock2l6c20bootsd <- sd (wthlock2l6c20boot)

eurmldlock2l6c20 <- mld.wtd(europe$wage2l6c20, europe$weight)
eurmldlock2l6c20boot[n] <- eurmldlock2l6c20
eurmldlock2l6c20bootmean <- mean (eurmldlock2l6c20boot)
eurmldlock2l6c20bootsd <- sd (eurmldlock2l6c20boot)

sharebtlock2l6c20boot [n] <-  (btlock2l6c20/eurmldlock2l6c20)*100
sharewthlock2l6c20boot [n] <-  (wthlock2l6c20/eurmldlock2l6c20)*100

sharebtlock2l6c20bootmean <-  mean (sharebtlock2l6c20boot)
sharebtlock2l6c20bootsd <-  sd (sharebtlock2l6c20boot)

sharewthlock2l6c20bootmean <-  mean (sharewthlock2l6c20boot)
sharewthlock2l6c20bootsd <-  sd (sharewthlock2l6c20boot)


### Now with the 2l loss and 6m closure 30 ####

mldlock2l6c30 <- sapply (countriesbt, function(X) wmldf (df = X, "wage2l6c30", "weight"))

meanwagelock2l6c30 <- sapply (countriesbt, function(X) wmeanf (df = X, "wage2l6c30", "weight"))

#between inequality
btlock2l6c30 <- mld.wtd(meanwagelock2l6c30, weights = wshare)
wthlock2l6c30 <- weighted.mean(mldlock2l6c30, w = wshare)

btlock2l6c30boot[n] <- btlock2l6c30
wthlock2l6c30boot[n] <- wthlock2l6c30
btlock2l6c30bootmean <- mean (btlock2l6c30boot)
btlock2l6c30bootsd <- sd (btlock2l6c30boot)
wthlock2l6c30bootmean <- mean (wthlock2l6c30boot)
wthlock2l6c30bootsd <- sd (wthlock2l6c30boot)

eurmldlock2l6c30 <- mld.wtd(europe$wage2l6c30, europe$weight)
eurmldlock2l6c30boot[n] <- eurmldlock2l6c30
eurmldlock2l6c30bootmean <- mean (eurmldlock2l6c30boot)
eurmldlock2l6c30bootsd <- sd (eurmldlock2l6c30boot)

sharebtlock2l6c30boot [n] <-  (btlock2l6c30/eurmldlock2l6c30)*100
sharewthlock2l6c30boot [n] <-  (wthlock2l6c30/eurmldlock2l6c30)*100

sharebtlock2l6c30bootmean <-  mean (sharebtlock2l6c30boot)
sharebtlock2l6c30bootsd <-  sd (sharebtlock2l6c30boot)

sharewthlock2l6c30bootmean <-  mean (sharewthlock2l6c30boot)
sharewthlock2l6c30bootsd <-  sd (sharewthlock2l6c30boot)


### Now with the 2l loss and 6m closure 40 ####

mldlock2l6c40 <- sapply (countriesbt, function(X) wmldf (df = X, "wage2l6c40", "weight"))

meanwagelock2l6c40 <- sapply (countriesbt, function(X) wmeanf (df = X, "wage2l6c40", "weight"))

#between inequality
btlock2l6c40 <- mld.wtd(meanwagelock2l6c40, weights = wshare)
wthlock2l6c40 <- weighted.mean(mldlock2l6c40, w = wshare)

btlock2l6c40boot[n] <- btlock2l6c40
wthlock2l6c40boot[n] <- wthlock2l6c40
btlock2l6c40bootmean <- mean (btlock2l6c40boot)
btlock2l6c40bootsd <- sd (btlock2l6c40boot)
wthlock2l6c40bootmean <- mean (wthlock2l6c40boot)
wthlock2l6c40bootsd <- sd (wthlock2l6c40boot)

eurmldlock2l6c40 <- mld.wtd(europe$wage2l6c40, europe$weight)
eurmldlock2l6c40boot[n] <- eurmldlock2l6c40
eurmldlock2l6c40bootmean <- mean (eurmldlock2l6c40boot)
eurmldlock2l6c40bootsd <- sd (eurmldlock2l6c40boot)

sharebtlock2l6c40boot [n] <-  (btlock2l6c40/eurmldlock2l6c40)*100
sharewthlock2l6c40boot [n] <-  (wthlock2l6c40/eurmldlock2l6c40)*100

sharebtlock2l6c40bootmean <-  mean (sharebtlock2l6c40boot)
sharebtlock2l6c40bootsd <-  sd (sharebtlock2l6c40boot)

sharewthlock2l6c40bootmean <-  mean (sharewthlock2l6c40boot)
sharewthlock2l6c40bootsd <-  sd (sharewthlock2l6c40boot)




#And now the increases


inc2leurboot [n] <- ((eurmldlock2l- eurmldlockbl)/eurmldlockbl)*100
inc2l6c20eurboot [n] <- ((eurmldlock2l6c20- eurmldlockbl)/eurmldlockbl)*100
inc2l6c30eurboot [n] <- ((eurmldlock2l6c30- eurmldlockbl)/eurmldlockbl)*100
inc2l6c40eurboot [n] <- ((eurmldlock2l6c40- eurmldlockbl)/eurmldlockbl)*100


inc2lbtboot [n] <- ((btlock2l- btlockbl)/btlockbl)*100
inc2l6c20btboot [n] <- ((btlock2l6c20- btlockbl)/btlockbl)*100
inc2l6c30btboot [n] <- ((btlock2l6c30- btlockbl)/btlockbl)*100
inc2l6c40btboot [n] <- ((btlock2l6c40- btlockbl)/btlockbl)*100



inc2lwthboot [n] <- ((wthlock2l- wthlockbl)/wthlockbl)*100
inc2l6c20wthboot [n] <- ((wthlock2l6c20- wthlockbl)/wthlockbl)*100
inc2l6c30wthboot [n] <- ((wthlock2l6c30- wthlockbl)/wthlockbl)*100
inc2l6c40wthboot [n] <- ((wthlock2l6c40- wthlockbl)/wthlockbl)*100


incabs2leurboot [n] <- (eurmldlock2l- eurmldlockbl)
incabs2l6c20eurboot [n] <- (eurmldlock2l6c20- eurmldlockbl)
incabs2l6c30eurboot [n] <- (eurmldlock2l6c30- eurmldlockbl)
incabs2l6c40eurboot [n] <- (eurmldlock2l6c40- eurmldlockbl)



incabs2lbtboot [n] <- (btlock2l- btlockbl)
incabs2l6c20btboot [n] <- (btlock2l6c20- btlockbl)
incabs2l6c30btboot [n] <- (btlock2l6c30- btlockbl)
incabs2l6c40btboot [n] <- (btlock2l6c40- btlockbl)

incabs2lwthboot [n] <- (wthlock2l- wthlockbl)
incabs2l6c20wthboot [n] <- (wthlock2l6c20- wthlockbl)
incabs2l6c30wthboot [n] <- (wthlock2l6c30- wthlockbl)
incabs2l6c40wthboot [n] <- (wthlock2l6c40- wthlockbl)


inc2leurbootmean <-  mean (inc2leurboot)
inc2leurbootsd <-  sd (inc2leurboot)

inc2l6c20eurbootmean <-  mean (inc2l6c20eurboot)
inc2l6c20eurbootsd <-  sd (inc2l6c20eurboot)

inc2l6c30eurbootmean <-  mean (inc2l6c30eurboot)
inc2l6c30eurbootsd <-  sd (inc2l6c30eurboot)

inc2l6c40eurbootmean <-  mean (inc2l6c40eurboot)
inc2l6c40eurbootsd <-  sd (inc2l6c40eurboot)

inc2lbtbootmean <-  mean (inc2lbtboot)
inc2lbtbootsd <-  sd (inc2lbtboot)

inc2l6c20btbootmean <-  mean (inc2l6c20btboot)
inc2l6c20btbootsd <-  sd (inc2l6c20btboot)

inc2l6c30btbootmean <-  mean (inc2l6c30btboot)
inc2l6c30btbootsd <-  sd (inc2l6c30btboot)

inc2l6c40btbootmean <-  mean (inc2l6c40btboot)
inc2l6c40btbootsd <-  sd (inc2l6c40btboot)

inc2lwthbootmean <-  mean (inc2lwthboot)
inc2lwthbootsd <-  sd (inc2lwthboot)

inc2l6c20wthbootmean <-  mean (inc2l6c20wthboot)
inc2l6c20wthbootsd <-  sd (inc2l6c20wthboot)

inc2l6c30wthbootmean <-  mean (inc2l6c30wthboot)
inc2l6c30wthbootsd <-  sd (inc2l6c30wthboot)

inc2l6c40wthbootmean <-  mean (inc2l6c40wthboot)
inc2l6c40wthbootsd <-  sd (inc2l6c40wthboot)

incabs2leurbootmean <-  mean (incabs2leurboot)
incabs2leurbootsd <-  sd (incabs2leurboot)

incabs2l6c20eurbootmean <-  mean (incabs2l6c20eurboot)
incabs2l6c20eurbootsd <-  sd (incabs2l6c20eurboot)

incabs2l6c30eurbootmean <-  mean (incabs2l6c30eurboot)
incabs2l6c30eurbootsd <-  sd (incabs2l6c30eurboot)

incabs2l6c40eurbootmean <-  mean (incabs2l6c40eurboot)
incabs2l6c40eurbootsd <-  sd (incabs2l6c40eurboot)

incabs2lbtbootmean <-  mean (incabs2lbtboot)
incabs2lbtbootsd <-  sd (incabs2lbtboot)

incabs2l6c20btbootmean <-  mean (incabs2l6c20btboot)
incabs2l6c20btbootsd <-  sd (incabs2l6c20btboot)

incabs2l6c30btbootmean <-  mean (incabs2l6c30btboot)
incabs2l6c30btbootsd <-  sd (incabs2l6c30btboot)

incabs2l6c40btbootmean <-  mean (incabs2l6c40btboot)
incabs2l6c40btbootsd <-  sd (incabs2l6c40btboot)

incabs2lwthbootmean <-  mean (incabs2lwthboot)
incabs2lwthbootsd <-  sd (incabs2lwthboot)

incabs2l6c20wthbootmean <-  mean (incabs2l6c20wthboot)
incabs2l6c20wthbootsd <-  sd (incabs2l6c20wthboot)

incabs2l6c30wthbootmean <-  mean (incabs2l6c30wthboot)
incabs2l6c30wthbootsd <-  sd (incabs2l6c30wthboot)

incabs2l6c40wthbootmean <-  mean (incabs2l6c40wthboot)
incabs2l6c40wthbootsd <-  sd (incabs2l6c40wthboot)

}



# now we get the point estimates

europe <- bind_rows(countriesbt) #and create europe again

weighteurope <- sum(europe[["weight"]])
wshare <- sapply (countriesbt, function(X)  { sum(X[["weight"]])/weighteurope})

mldlockbl <- sapply (countriesbt, function(X) wmldf (df = X, "wage", "weight"))

meanwagelockbl <- sapply (countriesbt, function(X) wmeanf (df = X, "wage", "weight"))

#between inequality
btlockbl <- mld.wtd(meanwagelockbl, weights = wshare)
#within
wthlockbl <- weighted.mean(mldlockbl, w = wshare)


eurmldlockbl <- mld.wtd(europe$wage, europe$weight)

sharebtlockbl <-  (btlockbl/eurmldlockbl)*100
sharewthlockbl <-  (wthlockbl/eurmldlockbl)*100


### now with the 2l loss

mldlock2l <- sapply (countriesbt, function(X) wmldf (df = X, "wage2l", "weight"))

meanwagelock2l <- sapply (countriesbt, function(X) wmeanf (df = X, "wage2l", "weight"))

#between inequality
btlock2l <- mld.wtd(meanwagelock2l, weights = wshare)
#within
wthlock2l <- weighted.mean(mldlock2l, w = wshare)


eurmldlock2l <- mld.wtd(europe$wage2l, europe$weight)

sharebtlock2l <-  (btlock2l/eurmldlock2l)*100
sharewthlock2l <-  (wthlock2l/eurmldlock2l)*100

## Now with the 2l loss and 6m closure 20 ####

mldlock2l6c20 <- sapply (countriesbt, function(X) wmldf (df = X, "wage2l6c20", "weight"))

meanwagelock2l6c20 <- sapply (countriesbt, function(X) wmeanf (df = X, "wage2l6c20", "weight"))

#between inequality
btlock2l6c20 <- mld.wtd(meanwagelock2l6c20, weights = wshare)
wthlock2l6c20 <- weighted.mean(mldlock2l6c20, w = wshare)

eurmldlock2l6c20 <- mld.wtd(europe$wage2l6c20, europe$weight)

sharebtlock2l6c20 <-  (btlock2l6c20/eurmldlock2l6c20)*100
sharewthlock2l6c20 <-  (wthlock2l6c20/eurmldlock2l6c20)*100

### Now with the 2l loss and 6m closure 30 ####

mldlock2l6c30 <- sapply (countriesbt, function(X) wmldf (df = X, "wage2l6c30", "weight"))

meanwagelock2l6c30 <- sapply (countriesbt, function(X) wmeanf (df = X, "wage2l6c30", "weight"))

#between inequality
btlock2l6c30 <- mld.wtd(meanwagelock2l6c30, weights = wshare)
#within
wthlock2l6c30 <- weighted.mean(mldlock2l6c30, w = wshare)

#overall ineq in europe
eurmldlock2l6c30 <- mld.wtd(europe$wage2l6c30, europe$weight)


sharebtlock2l6c30 <-  (btlock2l6c30/eurmldlock2l6c30)*100
sharewthlock2l6c30 <-  (wthlock2l6c30/eurmldlock2l6c30)*100


### Now with the 2l loss and 6m closure 40 ####

mldlock2l6c40 <- sapply (countriesbt, function(X) wmldf (df = X, "wage2l6c40", "weight"))

meanwagelock2l6c40 <- sapply (countriesbt, function(X) wmeanf (df = X, "wage2l6c40", "weight"))

#between inequality
btlock2l6c40 <- mld.wtd(meanwagelock2l6c40, weights = wshare)
#within
wthlock2l6c40 <- weighted.mean(mldlock2l6c40, w = wshare)

eurmldlock2l6c40 <- mld.wtd(europe$wage2l6c40, europe$weight)

sharebtlock2l6c40 <-  (btlock2l6c40/eurmldlock2l6c40)*100
sharewthlock2l6c40 <-  (wthlock2l6c40/eurmldlock2l6c40)*100


### And the direct estimation of the increases

#overall inequality
inc2leur <- ((eurmldlock2l- eurmldlockbl)/eurmldlockbl)*100
inc2l6c20eur <- ((eurmldlock2l6c20- eurmldlockbl)/eurmldlockbl)*100
inc2l6c30eur <- ((eurmldlock2l6c30- eurmldlockbl)/eurmldlockbl)*100
inc2l6c40eur <- ((eurmldlock2l6c40- eurmldlockbl)/eurmldlockbl)*100

#between ineq
inc2lbt <- ((btlock2l- btlockbl)/btlockbl)*100
inc2l6c20bt <- ((btlock2l6c20- btlockbl)/btlockbl)*100
inc2l6c30bt <- ((btlock2l6c30- btlockbl)/btlockbl)*100
inc2l6c40bt <- ((btlock2l6c40- btlockbl)/btlockbl)*100

#within ineq
inc2lwth <- ((wthlock2l- wthlockbl)/wthlockbl)*100
inc2l6c20wth <- ((wthlock2l6c20- wthlockbl)/wthlockbl)*100
inc2l6c30wth <- ((wthlock2l6c30- wthlockbl)/wthlockbl)*100
inc2l6c40wth <- ((wthlock2l6c40- wthlockbl)/wthlockbl)*100

#absolute increases#

#overall
incabs2leur <- (eurmldlock2l- eurmldlockbl)
incabs2l6c20eur <- (eurmldlock2l6c20- eurmldlockbl)
incabs2l6c30eur <- (eurmldlock2l6c30- eurmldlockbl)
incabs2l6c40eur <- (eurmldlock2l6c40- eurmldlockbl)

#between ineq
incabs2lbt <- (btlock2l- btlockbl)
incabs2l6c20bt <- (btlock2l6c20- btlockbl)
incabs2l6c30bt <- (btlock2l6c30- btlockbl)
incabs2l6c40bt <- (btlock2l6c40- btlockbl)

#within ineq
incabs2lwth <- (wthlock2l- wthlockbl)
incabs2l6c20wth <- (wthlock2l6c20- wthlockbl)
incabs2l6c30wth <- (wthlock2l6c30- wthlockbl)
incabs2l6c40wth <- (wthlock2l6c40- wthlockbl)



#### Results (using point estimates and sd from bootstrap)

baselineresults <- c(eurmldlockbl, eurmldlockblbootsd, btlockbl, btlockblbootsd, sharebtlockbl, sharebtlockblbootsd, wthlockbl, wthlockblbootsd, sharewthlockbl, sharewthlockblbootsd)

baselineresultsbootmean <- c(eurmldlockblbootmean, eurmldlockblbootsd, btlockblbootmean, btlockblbootsd, sharebtlockblbootmean, sharebtlockblbootsd, wthlockblbootmean, wthlockblbootsd, sharewthlockblbootmean, sharewthlockblbootsd)

lock2lresults <- c(eurmldlock2l, eurmldlock2lbootsd, btlock2l, btlock2lbootsd, sharebtlock2l, sharebtlock2lbootsd, wthlock2l, wthlock2lbootsd, sharewthlock2l, sharewthlock2lbootsd)

lock2lresultsbootmean <- c(eurmldlock2lbootmean, eurmldlock2lbootsd, btlock2lbootmean, btlock2lbootsd, sharebtlock2lbootmean, sharebtlock2lbootsd, wthlock2lbootmean, wthlock2lbootsd, sharewthlock2lbootmean, sharewthlock2lbootsd)

lock2l6c20results <- c(eurmldlock2l6c20, eurmldlock2l6c20bootsd, btlock2l6c20, btlock2l6c20bootsd, sharebtlock2l6c20, sharebtlock2l6c20bootsd, wthlock2l6c20, wthlock2l6c20bootsd, sharewthlock2l6c20, sharewthlock2l6c20bootsd)

lock2l6c30results <- c(eurmldlock2l6c30, eurmldlock2l6c30bootsd, btlock2l6c30, btlock2l6c30bootsd, sharebtlock2l6c30, sharebtlock2l6c30bootsd, wthlock2l6c30, wthlock2l6c30bootsd, sharewthlock2l6c30, sharewthlock2l6c30bootsd)

lock2l6c40results <- c(eurmldlock2l6c40, eurmldlock2l6c40bootsd, btlock2l6c40, btlock2l6c40bootsd, sharebtlock2l6c40, sharebtlock2l6c40bootsd, wthlock2l6c40, wthlock2l6c40bootsd, sharewthlock2l6c40, sharewthlock2l6c40bootsd)

results1 <- as.data.frame(rbind(baselineresults,  lock2lresults, lock2l6c20results, lock2l6c30results, lock2l6c40results))

names(results1) <- c("mld", "se", "bt", "se", "share_bt",  "se", "wth", "se", "share_wth", "se")

#We save results for Table 4 (first part)
write_csv(results1, "table4_prs_deco_mld_boot_se_with_shares_6m.csv")


#not the table of the increases at each scenario

incabs2l <- c(incabs2leur, incabs2leurbootsd, incabs2lbt, incabs2lbtbootsd, incabs2lwth, incabs2lwthbootsd)

inc2l <- c(inc2leur, inc2leurbootsd, inc2lbt, inc2lbtbootsd, inc2lwth, inc2lwthbootsd)

incabs2l6c20 <- c(incabs2l6c20eur, incabs2l6c20eurbootsd, incabs2l6c20bt, incabs2l6c20btbootsd, incabs2l6c20wth, incabs2l6c20wthbootsd)

inc2l6c20 <- c(inc2l6c20eur, inc2l6c20eurbootsd, inc2l6c20bt, inc2l6c20btbootsd, inc2l6c20wth, inc2l6c20wthbootsd)

incabs2l6c30 <- c(incabs2l6c30eur, incabs2l6c30eurbootsd, incabs2l6c30bt, incabs2l6c30btbootsd, incabs2l6c30wth, incabs2l6c30wthbootsd)

inc2l6c30 <- c(inc2l6c30eur, inc2l6c30eurbootsd, inc2l6c30bt, inc2l6c30btbootsd, inc2l6c30wth, inc2l6c30wthbootsd)

incabs2l6c40 <- c(incabs2l6c40eur, incabs2l6c40eurbootsd, incabs2l6c40bt, incabs2l6c40btbootsd, incabs2l6c40wth, incabs2l6c40wthbootsd)

inc2l6c40 <- c(inc2l6c40eur, inc2l6c40eurbootsd, inc2l6c40bt, inc2l6c40btbootsd, inc2l6c40wth, inc2l6c40wthbootsd)

resultsinc <- as.data.frame(rbind(incabs2l, inc2l, incabs2l6c20, inc2l6c20, incabs2l6c30, inc2l6c30, incabs2l6c40, inc2l6c40))

names(resultsinc) <- c("inc_ineq", "se", "inc_bt", "se", "inc_wth",  "se")

#We save the results for Table 4
write_csv(resultsinc, "table4_prs_deco_mld_boot_se_increases_6m.csv")
