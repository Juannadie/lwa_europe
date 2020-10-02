#load all packages that we might use throughout the code
library(tidyverse)
library(readstata13)
library(reshape2)
library(dineq)
library(ggrepel)
library(matrixStats)
library(leaflet)
library(leaflet.providers)
library(ggplot2)
library(maptools)
library(rgeos)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)
library(mapproj)
library(sf)
library(cowplot)
library(scales)
library(RColorBrewer)
library(mapproj)
library(zoo)
library(plotly)
library(gapminder)
library(ggrepel)
library(ggpubr)
library(reldist)

options ("scipen"=10000, "digits"= 6)
set.seed(8000)



#setwd("WORKING DIRECTORY") #Run to set directory where  files are stored

temp = list.files(pattern="*P.dta") #creates a list with all the personal files in the eu-silc directory directory
countries <- lapply(temp, read.dta13) #reads the list (note, alphabetical order)


#we create the country_id variable in each country
countries <- lapply(countries, function(X) {
  X[["country"]] <- X[["pb020"]]
  X
})

#we switch Greece code to GR in EU-SILC
countries <- lapply(countries, function(X) {
  X[["country"]][X[["country"]] == 'EL'] <- 'GR'
  X
})

namescountries <- sapply(countries, function(x) {sample(x[["country"]], size = 1)})
names(countries) <- namescountries #We get the names of the countries from the variables

countries <- countries[order(names(countries))] #order the list alphabetically

allcountries <- countries

countries <- countries[c(1:20,22:26,28:31)] #We remove MT (element 21 in our list), since we could not obtain TW for occupations in that country and element 27 RS (Serbia) since we do not have TW information either.
#Note this assumes you have originally loaded the full list of countries available in EU-SILC 2018
names(countries) #renaming countries



#LOAD TELEWORKING INDICES



twisco2d <- read_csv("tw_isco_2d_countries.csv") #2digit isco
twisco1d <- read_csv("tw_isco_1d_countries.csv") #1digit isco

#2 digit isco
twisco2dlong <- melt(twisco2d, id.vars = "isco2d")
names(twisco2dlong) <- c("isco2d", "country", "teleworking")

#1 digit isco
twisco1dlong <- melt(twisco1d, id.vars = "isco1d")
names(twisco1dlong) <- c("isco1d", "country", "teleworking")



#MERGE TELEWORKING, ESSENTIAL AND CLOSE INDICES WITH MAIN DATA

countries <- lapply(countries, function(X) {
  X <- X[!is.na(X[["pl051"]]),]
  X <- X[X[["pl051"]] != 0,]
  X
}) #we keep only non-missing occupation observations and non-armed forces occupations


#create variables with compatible names for merging

#occupation
countries <- lapply(countries, function(X) {
  X[["isco2d"]] <- X[["pl051"]]
  X
})

countries <- lapply(countries, function(X) {
  X[["isco1d"]] <- ifelse(X[["pl051"]] >= 10, trunc(X[["pl051"]]/10), X[["pl051"]])
  X
}) #We create ISCO 1d (from ISCO 2d)

countries <- lapply(countries, function(X) {
  X[["isco1d"]] <- ifelse(X[["pl051"]] < 10 & X[["pb020"]] !=("DE") & X[["pb020"]] !=("SI"), trunc(X[["pl051"]]/10), X[["isco1d"]])
  X
}) #We also convert to zeros in isco 1d obs <10 in countries that have isco2d.



countries <- lapply(countries, function(X) {
  X <- X[X[["isco1d"]] > 0,]
  X
}) #we also remove observations = 0 in 1 in isco 1d (we exclude armed forces occupations)


#industry
countries <- lapply(countries, function(X) {
  X[["nace"]] <- X[["pl111"]]
  X
})



#we merge the main data with the teleworking index
#for countries with 2d information about teleworking

names(countries)

#We filter for the ones that have 2digit ISCO in both our TW and EU-SILC
countries2dtw <- countries[c(1:2,4:6,8:22,24:26,28:29)]  # All countries except BG, DE, PL, SI
names(countries2dtw)


#We filter for the ones that have 1digit ISCO in both our TW and EU-SILC
countries1dtw <- countries[c(3, 7, 23, 27)] #BG, DE, PL, SI
names(countries1dtw)


#function to merge
merge2d <- function(df1, df2) {
  a <- merge(df1, df2, by = c("isco2d", "country"))
  a
}

countries2dtw <- lapply(countries2dtw, function (X) (merge2d (df1 = X, df2 = twisco2dlong))) #we merge each of this countries

#function to merge
merge1d <- function(df1, df2) {
  a <- merge(df1, df2, by = c("isco1d", "country"))
  a
}

countries1dtw <- lapply(countries1dtw, function (X) (merge1d (df1 = X, df2 = twisco1dlong))) #we merge each of this countries

#we reaggregate all countries in the list
countries <- c(countries2dtw, countries1dtw)
countries <- countries[order(names(countries))] #order back the list alphabetically
names(countries)


#LOAD ESSENTIAL AND CLOSED INDICES

 #Directory where the indices are stored
essentialindex21 <- read_csv("essential_index21.csv") #2digit isco - 1 digit nace
closedindex21 <- read_csv("closed_index21.csv")
essentialindex11 <- read_csv("essential_index11.csv") #1digit isco - 1 digit nace
closedindex11 <- read_csv("closed_index11.csv")
essentialindex2 <- read_csv("essential_index2.csv") #2digit isco only
closedindex2 <- read_csv("closed_index2.csv")

#now convert to long format; we need that to merge by two variables

essential21long <- melt(essentialindex21, id.vars = "isco2d")
names(essential21long) <- c("isco2d", "nace", "essential_index")

closed21long <- melt(closedindex21, id.vars = "isco2d")
names(closed21long) <- c("isco2d", "nace", "closed_index")

#And merge essential and closed index in one "long format file"
ei_ci_index_long_2d_isco_1d_nace <- merge(essential21long, closed21long, by = c("isco2d", "nace"))


#now the same for 1 digit ISCO - 1 digit NACE coding
essential11long <- melt(essentialindex11, id.vars = "isco1d")
names(essential11long) <- c("isco1d", "nace", "essential_index")

closed11long <- melt(closedindex11, id.vars = "isco1d")
names(closed11long) <- c("isco1d", "nace", "closed_index")

#and merge essential and closed indices in one long file
ei_ci_index_long_1d_isco_1d_nace <- merge(essential11long, closed11long, by = c("isco1d", "nace"))


#now for only isco 2 digit
#no need to reshape, only one variable already
names(essentialindex2) <- c("isco2d", "essential_index")
names(closedindex2) <- c("isco2d","closed_index")

#And merge ((no industries))
essential_closed_index_long_only_isco <- merge(essentialindex2, closedindex2, by = c("isco2d"))


#now adding essential and closed index

#We filter for the countries that have 2digit ISCO and 1 digit NACE
countries1 <- countries[c(1:2,4:6,9:22,24:26,28:29)] #all except BG, DE, PL, SI and DK
names(countries1)

#function to merge
merge1 <- function(df1, df2) {
  a <- merge(df1, df2, by = c("isco2d", "nace"))
  a
}

countries1 <- lapply(countries1, function (X) (merge1 (df1 = X, df2 = ei_ci_index_long_2d_isco_1d_nace))) #we merge each of this countries


#Now for the countries for which we only have 1-digit tw
#We filter for the ones we merge BG, DE, PL, SI (that only have 1digit ISCO and 1 digit NACE)
countries2 <- countries[c(3, 7, 23, 27)]
names(countries2)

merge2 <- function(df1, df2) {
  a <- merge(df1, df2, by = c("isco1d", "nace"))
  a
}

countries2 <- lapply(countries2, function (X) (merge2 (df1 = X, df2 = ei_ci_index_long_1d_isco_1d_nace)))



#Now for Denmark (DK does not have NACE info in EU-SILC (most are missing))
countries3 <- countries[c(8)]
names3 <- namescountries[c(8)]


merge3 <- function(df1, df2) {
  a <- merge(df1, df2, by = c("isco2d"))
  a
}

countries3 <- lapply(countries3, function(x) {merge3(x, essential_closed_index_long_only_isco)})

countries <- c(countries1, countries2, countries3)
countries <- countries[order(names(countries))] #order back the list alphabetically
names(countries)


saveRDS(countries, file = "countries_1.rds") #saves the data in working directory

