



countries <- readRDS(file = "countries_1.rds") #reads data from previous step


#We rename/create the variables we need for all countries
countries <- lapply(countries, function(X) {
  X[["weight"]] <- X[["pb040"]]
  X
})

countries <- lapply(countries, function(X) {
  X[["age"]] <- X[["px020"]]
  X
})


countries <- lapply(countries, function(X) {
  X[["wage"]] <- rowSums(X[c("py010g", "py020g", "py050g")], na.rm = T)
  X
}) #wage for employees and self-employed, in-cash or in-kind.


### We keep the ones in working age, active or working, with occupation information (unavailable in some cases for occupations 63 and 95, so we drop them), and with wage information and  tw information

countries <- lapply(countries, function(X) {
  X <- X[(X[["age"]]>= 16 & X[["pl031"]]<=4 & !is.na(X[["isco2d"]]) & (X[["isco2d"]]!= 63) & X[["isco2d"]]!= 95  & X[["wage"]]>0 & !is.na(X[["teleworking"]])),]
  X
})


#now let's create variables for education, gender and type of contract

countries <- lapply(countries, function(X) {

#education
X[["lowedu"]] <- NULL
X[["lowedu"]] <- 0
X[["lowedu"]][X[["pe040"]] <= 200] <- 1

X[["midedu"]] <- 0
X[["midedu"]][X[["pe040"]] > 200 & X[["pe040"]] < 500] <- 1

X[["highedu"]] <- 0
X[["highedu"]][X[["pe040"]] >=500] <-1

#gender
X[["sex"]] <- X[["pb150"]] #1 male, 2 female

#and create dummies
X[["male"]] <- 0
X[["male"]][X[["sex"]] == 1] <-1

X[["female"]] <- 0
X[["female"]][X[["sex"]] == 2] <-1


#type of contract
X[["contract"]] <- X[["pl140"]] #1 permanent, 2 temporary

#And the dummies
X[["permanent"]] <- 0
X[["permanent"]][X[["contract"]] == 1] <-1

X[["temporary"]] <- 0
X[["temporary"]][X[["contract"]] == 2] <-1


#type of contract
X[["job"]][X[["pl031"]] %in% c(1,3)] <- 1
X[["job"]][X[["pl031"]] %in% c(2,4)] <- 2

#and with the dummies
X[["full"]] <- 0
X[["full"]][X[["job"]] == 1 ] <-1

X[["part"]] <- 0
X[["part"]][X[["job"]] == 2 ] <-1


X
})


saveRDS(countries, file = "countries_2.rds")
