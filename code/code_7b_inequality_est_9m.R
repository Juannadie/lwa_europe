

#load data from step 3
countries <- readRDS(file = "countries_3.rds")

namescountries <- names (countries) #we get the names of the countries

# We set up some functions we will use

#gini
wginif <- function(df, var, wg) {
  gini <- gini.wtd (df[[var]], df[[wg]])
  gini
}

#mld
wmldf <- function(df, var, wg) {
  mld <- mld.wtd (df[[var]], df[[wg]])
  mld
}


#mean general function
wmeanf <- function(df, var, wg) {
  mean <- weighted.mean(df[[var]], df[[wg]], na.rm = T)
  mean
}



#Simulaciónes

##### Let's get the increases by bootstrap for all scenarios

ginibaselinedf <- vector()
gini2ldf <- vector()
gini2lincreasedf<- vector ()
gini2lincreasepcdf <- vector ()


gini2l9c20df <- vector()
gini2l9c20increasedf <- vector ()
gini2l9c20increasepcdf <- vector ()

gini2l9c30df <- vector()
gini2l9c30increasedf <- vector ()
gini2l9c30increasepcdf <- vector ()


gini2l9c40df <- vector()
gini2l9c40increasedf <- vector ()
gini2l9c40increasepcdf <- vector ()



for (n in 1:100)
{countriesboot <- lapply (countries, function(df) df[sample(nrow(df), replace=TRUE),])


ginibaseline <- sapply (countriesboot, function(R) gini.wtd (x = R[["wage"]], weights = R[["weight"]]))
gini2l <- sapply (countriesboot, function(R) wginif (df = R, "wage2l", "weight"))
gini2l9c20 <- sapply (countriesboot, function(R) wginif (df = R, "wage2l9c20", "weight"))
gini2l9c30 <- sapply (countriesboot, function(R) wginif (df = R, "wage2l9c30", "weight"))
gini2l9c40 <- sapply (countriesboot, function(R) wginif (df = R, "wage2l9c40", "weight"))



#ginibaselineboot[n] <- ginibaseline
ginibaselinedf <- cbind(ginibaselinedf, ginibaseline)
gini2ldf <- cbind(gini2ldf, gini2l)
gini2l9c20df <- cbind(gini2l9c20df, gini2l9c20)
gini2l9c30df <- cbind(gini2l9c30df, gini2l9c30)
gini2l9c40df <- cbind(gini2l9c40df, gini2l9c40)

gini2lincreaseboot <- gini2l - ginibaseline
gini2lincreasedf <- cbind(gini2lincreasedf, gini2lincreaseboot)

gini2lincreasebootpc <- ((gini2l - ginibaseline)/ginibaseline)*100
gini2lincreasepcdf <- cbind(gini2lincreasepcdf, gini2lincreasebootpc)


gini2l9c20increaseboot <- gini2l9c20 - ginibaseline
gini2l9c20increasedf <- cbind(gini2l9c20increasedf, gini2l9c20increaseboot)

gini2l9c20increasebootpc <- ((gini2l9c20 - ginibaseline)/ginibaseline)*100
gini2l9c20increasepcdf <- cbind(gini2l9c20increasepcdf, gini2l9c20increasebootpc)


gini2l9c30increaseboot <- gini2l9c30 - ginibaseline
gini2l9c30increasedf <- cbind(gini2l9c30increasedf, gini2l9c30increaseboot)

gini2l9c30increasebootpc <- ((gini2l9c30 - ginibaseline)/ginibaseline)*100
gini2l9c30increasepcdf <- cbind(gini2l9c30increasepcdf, gini2l9c30increasebootpc)

gini2l9c40increaseboot <- gini2l9c40 - ginibaseline
gini2l9c40increasedf <- cbind(gini2l9c40increasedf, gini2l9c40increaseboot)

gini2l9c40increasebootpc <- ((gini2l9c40 - ginibaseline)/ginibaseline)*100
gini2l9c40increasepcdf <- cbind(gini2l9c40increasepcdf, gini2l9c40increasebootpc)


# Now get mean bootstrap value and SD

#ginibaselinebootmean <- rowMeans2(ginibaselinedf)
ginibaselinebootmean <- apply(ginibaselinedf, 1, mean)
ginibaselinebootsd <- apply(ginibaselinedf, 1, sd)

gini2lbootmean <- apply(gini2ldf, 1, mean)
gini2lbootsd <- apply(gini2ldf, 1, sd)

gini2l9c20bootmean <- apply(gini2l9c20df, 1, mean)
gini2l9c20bootsd <- apply(gini2l9c20df, 1, sd)

gini2l9c30bootmean <- apply(gini2l9c30df, 1, mean)
gini2l9c30bootsd <- apply(gini2l9c30df, 1, sd)

gini2l9c40bootmean <- apply(gini2l9c40df, 1, mean)
gini2l9c40bootsd <- apply(gini2l9c40df, 1, sd)



#get mean bootstrap increase and, more importantly, bootstrap sd (pc means in percentage)

#lockdown 2 months
#absolute increase
gini2lincreasebootmean <- apply(gini2lincreasedf, 1, mean)
gini2lincreasebootsd <- apply(gini2lincreasedf, 1, sd)

#percentage increase
gini2lincreasepcbootmean <- apply(gini2lincreasepcdf, 1, mean)
gini2lincreasepcbootsd <- apply(gini2lincreasepcdf, 1, sd)

#l2 + 9c 20%
gini2l9c20increasebootmean <- apply(gini2l9c20increasedf, 1, mean)
gini2l9c20increasebootsd <- apply(gini2l9c20increasedf, 1, sd)

gini2l9c20increasepcbootmean <- apply(gini2l9c20increasepcdf, 1, mean)
gini2l9c20increasepcbootsd <- apply(gini2l9c20increasepcdf, 1, sd)

#l2 + 9c 30%
gini2l9c30increasebootmean <- apply(gini2l9c30increasedf, 1, mean)
gini2l9c30increasebootsd <- apply(gini2l9c30increasedf, 1, sd)

gini2l9c30increasepcbootmean <- apply(gini2l9c30increasepcdf, 1, mean)
gini2l9c30increasepcbootsd <- apply(gini2l9c30increasepcdf, 1, sd)

#l2 + 9c 40%
gini2l9c40increasebootmean <- apply(gini2l9c40increasedf, 1, mean)
gini2l9c40increasebootsd <- apply(gini2l9c40increasedf, 1, sd)

gini2l9c40increasepcbootmean <- apply(gini2l9c40increasepcdf, 1, mean)
gini2l9c40increasepcbootsd <- apply(gini2l9c40increasepcdf, 1, sd)

}


#we could use the mean of the bootstrap as point estimates, but we use the estimates in the original sample

ginibaseline <- sapply (countries, function(R) gini.wtd (x = R[["wage"]], weights = R[["weight"]]))
gini2l <- sapply (countries, function(R) wginif (df = R, "wage2l", "weight"))
gini2l9c20 <- sapply (countries, function(R) wginif (df = R, "wage2l9c20", "weight"))
gini2l9c30 <- sapply (countries, function(R) wginif (df = R, "wage2l9c30", "weight"))
gini2l9c40 <- sapply (countries, function(R) wginif (df = R, "wage2l9c40", "weight"))



gini2lincrease <- gini2l - ginibaseline
gini2lincreasepc <- ((gini2l - ginibaseline)/ginibaseline)*100
gini2l9c20increase <- gini2l9c20 - ginibaseline
gini2l9c20increasepc <- ((gini2l9c20 - ginibaseline)/ginibaseline)*100
gini2l9c30increase <- gini2l9c30 - ginibaseline
gini2l9c30increasepc <- ((gini2l9c30 - ginibaseline)/ginibaseline)*100
gini2l9c40increase <- gini2l9c40 - ginibaseline
gini2l9c40increasepc <- ((gini2l9c40 - ginibaseline)/ginibaseline)*100



ginitableboot <- as.data.frame(cbind(namescountries, ginibaseline,ginibaselinebootsd,gini2l,gini2lbootsd ,gini2lincrease,gini2lincreasebootsd,gini2lincreasepc,gini2lincreasepcbootsd, gini2l9c20,gini2l9c20bootsd, gini2l9c20increase, gini2l9c20increasebootsd, gini2l9c20increasepc, gini2l9c20increasepcbootsd, gini2l9c30,gini2l9c30bootsd, gini2l9c30increase, gini2l9c30increasebootsd, gini2l9c30increasepc, gini2l9c30increasepcbootsd, gini2l9c40,gini2l9c40bootsd, gini2l9c40increase, gini2l9c40increasebootsd, gini2l9c40increasepc, gini2l9c40increasepcbootsd))


#Estimations

names(ginitableboot) <- c("Country", "Baseline", "SE", "Lockdown 2m", "SE", "Increase 2l", "SE", "Increase 2l pc", "SE", "Lockdown 2l9c20", "SE",  "Increase 2l9c20", "SE", "Increase 2l9c20 pc", "SE", "Lockdown 2l9c30", "SE",  "Increase 2l9c30", "SE", "Increase 2l9c30 pc", "SE", "Lockdown 2l9c40", "SE",  "Increase 2l9c40", "SE", "Increase 2l9c40 pc", "SE" )

#Write results for Table D2 (Appendix D)

write_csv(ginitableboot, "tableD2_prs_gini_index_increase_sd_eer_boot_9m.csv")
