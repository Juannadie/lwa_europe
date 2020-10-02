

countries <- readRDS(file = "countries_3.rds") #loads data from step 3

#weighted mean general function
wmeanf <- function(df, var, wg) {
  mean <- weighted.mean(df[[var]], df[[wg]], na.rm = T)
  mean
}

#weighted mean function by group (using dummy to group)
wmeanfg <- function(df, var, dummy, wg) {
  mean <- weighted.mean(df[[var]][df[[dummy]]==1], df[[wg]][df[[dummy]]==1], na.rm = T)
  mean
}


#Descriptive values for LWA (used for Table 1)

meanlwaall <- sapply(countries, function (X) wmeanf (df = X, var = "lwa", wg = "weight"))

meanlwamale <- sapply(countries, function (X) wmeanfg (df = X, var = "lwa", dummy = "male", wg = "weight"))

meanlwafemale <- sapply(countries, function (X) wmeanfg (df = X, var = "lwa", dummy = "female", wg = "weight"))

meanlwapermanent <- sapply(countries, function (X) wmeanfg (df = X, var = "lwa", dummy = "permanent", wg = "weight"))

meanlwatemporary <- sapply(countries, function (X) wmeanfg (df = X, var = "lwa", dummy = "temporary", wg = "weight"))

meanlwafull <- sapply(countries, function (X) wmeanfg (df = X, var = "lwa", dummy = "full", wg = "weight"))
meanlwapart <- sapply(countries, function (X) wmeanfg (df = X, var = "lwa", dummy = "part", wg = "weight"))

meanlwalow <- sapply(countries, function (X) wmeanfg (df = X, var = "lwa", dummy = "lowedu", wg = "weight"))
meanlwamid <- sapply(countries, function (X) wmeanfg (df = X, var = "lwa", dummy = "midedu", wg = "weight"))
meanlwahigh <- sapply(countries, function (X) wmeanfg (df = X, var = "lwa", dummy = "highedu", wg = "weight"))



countriesnames <- names(countries)
countriesnames <- as.data.frame(countriesnames)

resultscountrieslwa <- cbind(meanlwaall, meanlwamale, meanlwafemale, meanlwapermanent, meanlwatemporary, meanlwafull, meanlwapart, meanlwalow, meanlwamid, meanlwahigh)

resultscountrieslwadf <- as.data.frame(resultscountrieslwa)
resultscountrieslwadf <- cbind(countriesnames,resultscountrieslwa)

#Table 1
write_csv(resultscountrieslwadf, "table_1_descriptives_countries_grouped_lwa_prs.csv") #saves Table 1 in Working Directory



