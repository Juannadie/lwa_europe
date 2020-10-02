


#We load the file from step 3

countries <- readRDS(file = "countries_3.rds")


## and we introduce the change of wage loss for each scenario

countries <- lapply(countries, function(X) {

  X[["pcloss2l"]] <- ((X[["wage2l"]] - X[["wage"]])/X[["wage"]])*100
  X
})


countries <- lapply(countries, function(X) {

  X[["pcloss2l9c20"]] <- ((X[["wage2l9c20"]] - X[["wage"]])/X[["wage"]])*100
  X
})



countries <- lapply(countries, function(X) {

  X[["pcloss2l9c30"]] <- ((X[["wage2l9c30"]] - X[["wage"]])/X[["wage"]])*100
  X
})

countries <- lapply(countries, function(X) {

  X[["pcloss2l9c40"]] <- ((X[["wage2l9c40"]] - X[["wage"]])/X[["wage"]])*100
  X
})


######

#bootstrap

#Simulaciónes

##### Let's get the increases by bootstrap

### Now with the 2mcl loss and 4m closure ####

povgrowth2ldf <- vector()
povgrowth2l9c20df <- vector()
povgrowth2l9c30df <- vector()
povgrowth2l9c40df <- vector()

headcountpredf <- vector()

headcount2ldf <- vector ()
headcount2l9c20df <- vector()
headcount2l9c30df <- vector()
headcount2l9c40df <- vector()

headcountchange2ldf <- vector ()
headcountchange2l9c20df <- vector()
headcountchange2l9c30df <- vector()
headcountchange2l9c40df <- vector()

for (n in 1:100)
{countriesboot <- lapply (countries, function(df) df[sample(nrow(df), replace=TRUE),])


povgrowth2l <- sapply(countriesboot, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  a <- sum (X[["pcloss2l"]][X[["wage"]]<povline]*X[["weight"]][X[["wage"]]<povline]) / sum(X[["weight"]][X[["wage"]]<povline])
  a
})


povgrowth2l9c20 <- sapply(countriesboot, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  a <- sum (X[["pcloss2l9c20"]][X[["wage"]]<povline]*X[["weight"]][X[["wage"]]<povline]) / sum(X[["weight"]][X[["wage"]]<povline])
  a
})


povgrowth2l9c30 <- sapply(countriesboot, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  a <- sum (X[["pcloss2l9c30"]][X[["wage"]]<povline]*X[["weight"]][X[["wage"]]<povline]) / sum(X[["weight"]][X[["wage"]]<povline])
  a
})


povgrowth2l9c40 <- sapply(countriesboot, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  a <- sum (X[["pcloss2l9c40"]][X[["wage"]]<povline]*X[["weight"]][X[["wage"]]<povline]) / sum(X[["weight"]][X[["wage"]]<povline])
  a
})


#headcount index

headcountpre <- sapply(countriesboot, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  X[["povdummy"]]<- 0
  X[["povdummy"]][X[["wage"]]<povline] <- 1
  a <- sum (X[["povdummy"]]*X[["weight"]]) / sum(X[["weight"]])
  a
})

headcount2l <- sapply(countriesboot, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  X[["povdummy"]]<- 0
  X[["povdummy"]][X[["wage2l"]]<povline] <- 1
  a <- sum (X[["povdummy"]]*X[["weight"]]) / sum(X[["weight"]])
  a
})

headcountchange2l <- headcount2l - headcountpre


headcount2l9c20 <- sapply(countriesboot, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  X[["povdummy"]]<- 0
  X[["povdummy"]][X[["wage2l9c20"]]<povline] <- 1
  a <- sum (X[["povdummy"]]*X[["weight"]]) / sum(X[["weight"]])
  a
})

headcountchange2l9c20 <- headcount2l9c20 - headcountpre


headcount2l9c30 <- sapply(countriesboot, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  X[["povdummy"]]<- 0
  X[["povdummy"]][X[["wage2l9c30"]]<povline] <- 1
  a <- sum (X[["povdummy"]]*X[["weight"]]) / sum(X[["weight"]])
  a
})

headcountchange2l9c30 <- headcount2l9c30 - headcountpre


headcount2l9c40 <- sapply(countriesboot, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  X[["povdummy"]]<- 0
  X[["povdummy"]][X[["wage2l9c40"]]<povline] <- 1
  a <- sum (X[["povdummy"]]*X[["weight"]]) / sum(X[["weight"]])
  a
})

headcountchange2l9c40 <- headcount2l9c40 - headcountpre



#ginibaselineboot[n] <- ginibaseline
povgrowth2ldf <- cbind(povgrowth2ldf, povgrowth2l)
povgrowth2l9c20df <- cbind(povgrowth2l9c20df, povgrowth2l9c20)
povgrowth2l9c30df <- cbind(povgrowth2l9c30df, povgrowth2l9c30)
povgrowth2l9c40df <- cbind(povgrowth2l9c40df, povgrowth2l9c40)

headcountpredf <- cbind(headcountpredf, headcountpre)
headcount2ldf <- cbind(headcount2ldf, headcount2l)
headcount2l9c20df <- cbind(headcount2l9c20df, headcount2l9c20)
headcount2l9c30df <- cbind(headcount2l9c30df, headcount2l9c30)
headcount2l9c40df <- cbind(headcount2l9c40df, headcount2l9c40)

headcountchange2ldf <- cbind (headcountchange2ldf, headcountchange2l )
headcountchange2l9c20df <- cbind(headcountchange2l9c20df, headcountchange2l9c20)
headcountchange2l9c30df <- cbind(headcountchange2l9c30df, headcountchange2l9c30)
headcountchange2l9c40df <- cbind(headcountchange2l9c40df, headcountchange2l9c40)

# Now get mean bootstrap value and SD

povgrowth2ldfbootmean <- apply(povgrowth2ldf, 1, mean)
povgrowth2ldfbootsd <- apply(povgrowth2ldf, 1, sd)

povgrowth2l9c20dfbootmean <- apply(povgrowth2l9c20df, 1, mean)
povgrowth2l9c20dfbootsd <- apply(povgrowth2l9c20df, 1, sd)

povgrowth2l9c30dfbootmean <- apply(povgrowth2l9c30df, 1, mean)
povgrowth2l9c30dfbootsd <- apply(povgrowth2l9c30df, 1, sd)

povgrowth2l9c40dfbootmean <- apply(povgrowth2l9c40df, 1, mean)
povgrowth2l9c40dfbootsd <- apply(povgrowth2l9c40df, 1, sd)


headcountpredfbootmean <- apply(headcountpredf, 1, mean)
headcountpredfbootsd <- apply(headcountpredf, 1, sd)

headcount2ldfbootmean <- apply(headcount2ldf, 1, mean)
headcount2ldfbootsd <- apply(headcount2ldf, 1, sd)

headcount2l9c20dfbootmean <- apply(headcount2l9c20df, 1, mean)
headcount2l9c20dfbootsd <- apply(headcount2l9c20df, 1, sd)


headcount2l9c30dfbootmean <- apply(headcount2l9c30df, 1, mean)
headcount2l9c30dfbootsd <- apply(headcount2l9c30df, 1, sd)

headcount2l9c40dfbootmean <- apply(headcount2l9c40df, 1, mean)
headcount2l9c40dfbootsd <- apply(headcount2l9c40df, 1, sd)


headcountchange2ldfbootmean <- apply(headcountchange2ldf, 1, mean)
headcountchange2ldfbootsd <- apply(headcountchange2ldf, 1, sd)

headcountchange2l9c20dfbootmean <- apply(headcountchange2l9c20df, 1, mean)
headcountchange2l9c20dfbootsd <- apply(headcountchange2l9c20df, 1, sd)


headcountchange2l9c30dfbootmean <- apply(headcountchange2l9c30df, 1, mean)
headcountchange2l9c30dfbootsd <- apply(headcountchange2l9c30df, 1, sd)

headcountchange2l9c40dfbootmean <- apply(headcountchange2l9c40df, 1, mean)
headcountchange2l9c40dfbootsd <- apply(headcountchange2l9c40df, 1, sd)
}


#direct estimates (not bootstrap)


#We calculate the average change for people in poverty, defined as population below 60% of the median.


povgrowth2l <- sapply(countries, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  a <- sum (X[["pcloss2l"]][X[["wage"]]<povline]*X[["weight"]][X[["wage"]]<povline]) / sum(X[["weight"]][X[["wage"]]<povline])
  a
})

###

povgrowth2l <- sapply(countries, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  a <- sum (X[["pcloss2l"]][X[["wage"]]<povline]*X[["weight"]][X[["wage"]]<povline]) / sum(X[["weight"]][X[["wage"]]<povline])
  a
})


povgrowth2l9c20 <- sapply(countries, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  a <- sum (X[["pcloss2l9c20"]][X[["wage"]]<povline]*X[["weight"]][X[["wage"]]<povline]) / sum(X[["weight"]][X[["wage"]]<povline])
  a
})

povgrowth2l9c30 <- sapply(countries, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  a <- sum (X[["pcloss2l9c30"]][X[["wage"]]<povline]*X[["weight"]][X[["wage"]]<povline]) / sum(X[["weight"]][X[["wage"]]<povline])
  a
})


povgrowth2l9c40 <- sapply(countries, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  a <- sum (X[["pcloss2l9c40"]][X[["wage"]]<povline]*X[["weight"]][X[["wage"]]<povline]) / sum(X[["weight"]][X[["wage"]]<povline])
  a
})


#headcount index

headcountpre <- sapply(countries, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  X[["povdummy"]]<- 0
  X[["povdummy"]][X[["wage"]]<povline] <- 1
  a <- sum (X[["povdummy"]]*X[["weight"]]) / sum(X[["weight"]])
  a
})

headcount2l <- sapply(countries, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  X[["povdummy"]]<- 0
  X[["povdummy"]][X[["wage2l"]]<povline] <- 1
  a <- sum (X[["povdummy"]]*X[["weight"]]) / sum(X[["weight"]])
  a
})

headcountchange2l <- headcount2l - headcountpre

headcount2l9c20 <- sapply(countries, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  X[["povdummy"]]<- 0
  X[["povdummy"]][X[["wage2l9c20"]]<povline] <- 1
  a <- sum (X[["povdummy"]]*X[["weight"]]) / sum(X[["weight"]])
  a
})

headcountchange2l9c20 <- headcount2l9c20 - headcountpre



headcount2l9c30 <- sapply(countries, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  X[["povdummy"]]<- 0
  X[["povdummy"]][X[["wage2l9c30"]]<povline] <- 1
  a <- sum (X[["povdummy"]]*X[["weight"]]) / sum(X[["weight"]])
  a
})

headcountchange2l9c30 <- headcount2l9c30 - headcountpre


headcount2l9c40 <- sapply(countries, function(X) {

  povline <- 0.6*(wtd.quantile (X[["wage"]], q = 0.5, weight = X[["weight"]] ))
  X[["povdummy"]]<- 0
  X[["povdummy"]][X[["wage2l9c40"]]<povline] <- 1
  a <- sum (X[["povdummy"]]*X[["weight"]]) / sum(X[["weight"]])
  a
})

headcountchange2l9c40 <- headcount2l9c40 - headcountpre

###



namescountries <- names(countries)

#poverty results (mean poor growth, headcount rate, for 2 month lockdown and the adictional 9 month closure (range of closure 20-30-40%, ie. capacity 80-70-60%). Includes bootstrapped standard errors (SD)).

povresultsboot <- as.data.frame(cbind(namescountries, povgrowth2l,  povgrowth2ldfbootsd,  povgrowth2l9c20,  povgrowth2l9c20dfbootsd, povgrowth2l9c30,  povgrowth2l9c30dfbootsd, povgrowth2l9c40,  povgrowth2l9c40dfbootsd, headcountpre, headcountpredfbootsd, headcount2l, headcount2ldfbootsd,  headcount2l9c20,  headcount2l9c20dfbootsd, headcount2l9c30,  headcount2l9c30dfbootsd, headcount2l9c40,  headcount2l9c40dfbootsd, headcountchange2l, headcountchange2ldfbootsd,  headcountchange2l9c20,  headcountchange2l9c20dfbootsd, headcountchange2l9c30,  headcountchange2l9c30dfbootsd, headcountchange2l9c40,  headcountchange2l9c40dfbootsd))

#write results for Table D1 (Appendix D)

write_csv(povresultsboot, "table_D1_prs_povwloss_sd_boot_9m.csv")

