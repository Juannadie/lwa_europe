



countries <- readRDS(file = "countries_2.rds") #loads data from previous step


#we rename the essential and closed indices variables for shorter code

countries <- lapply(countries, function(X) {

  X[["e_i"]] <- X[["essential_index"]]
  X[["c_i"]] <- X[["closed_index"]]
  X[["tw"]] <- X[["teleworking"]]
  X
})


#### Calculation of the Lockdown Working Ability Index for each worker, based on their tw, closure and essentiality scores ####

countries <- lapply(countries, function(X) {

  #for non essential and non closed (default)
  X[["lwa"]] <- X[["tw"]]

  #for essential (e_i > 0)
  X[["lwa"]][X[["e_i"]] > 0] <- (X[["e_i"]][X[["e_i"]] > 0] + (1-X[["e_i"]][X[["e_i"]] > 0])*X[["tw"]][X[["e_i"]] > 0])

  #for closed (c_i > 0)
  X[["lwa"]][X[["c_i"]] > 0] <- (1-X[["c_i"]][X[["c_i"]] > 0])*X[["tw"]][X[["c_i"]] > 0]

  X
})

#### NOW THE WAGE LOSS FOR EACH SCENARIO #####

# 2 months
countries <- lapply(countries, function(df) {

  #2 month

  #we set the number of months of the lockdown

  lcklenght <- 2

  df[["wloss2l"]] <- df[["wage"]]*(lcklenght/12)*(1-(df[["lwa"]]))
  df[["wage2l"]] <-  df[["wage"]] -  df[["wloss2l"]]



  #2 month plus 6 closure with a 20% capacity decrease
  # we set the closure at 20% (activity at 80% of capacity) for 6 months

  capacitydecr <- 0.2 #capacity decrease
  clslenght <- 6 #number of months of closure

  #non-closed activities (c_i = 0)
  df[["wage2l6c20"]][df[["c_i"]] == 0] <- df[["wage2l"]][df[["c_i"]] == 0] #same wage as 2 month of lockdown


  #closed activities (c_i > 0)
  df[["wage2l6c20"]][df[["c_i"]] > 0] <- df[["wage2l"]][df[["c_i"]] > 0] - ( df[["wage"]][df[["c_i"]] > 0]*(clslenght/12)*(capacitydecr)*df[["c_i"]][df[["c_i"]] > 0])
  df

  #2 month plus 6 closure with a 30% capacity decrease

  capacitydecr <- 0.3 #capacity decrease
  clslenght <- 6 #number of months of closure

  #non-closed activities (c_i = 0)
  df[["wage2l6c30"]][df[["c_i"]] == 0] <- df[["wage2l"]][df[["c_i"]] == 0] #same wage as 2 month of lockdown


  #closed activities (c_i > 0)
  df[["wage2l6c30"]][df[["c_i"]] > 0] <- df[["wage2l"]][df[["c_i"]] > 0] - ( df[["wage"]][df[["c_i"]] > 0]*(clslenght/12)*(capacitydecr)*df[["c_i"]][df[["c_i"]] > 0])
  df


  #2 month plus 6 closure with a 40% capacity decrease

  capacitydecr <- 0.4 #capacity decrease
  clslenght <- 6 #number of months of closure

  #non-closed activities (c_i = 0)
  df[["wage2l6c40"]][df[["c_i"]] == 0] <- df[["wage2l"]][df[["c_i"]] == 0] #same wage as 2 month of lockdown

  #closed activities (c_i > 0)
  df[["wage2l6c40"]][df[["c_i"]] > 0] <- df[["wage2l"]][df[["c_i"]] > 0] - ( df[["wage"]][df[["c_i"]] > 0]*(clslenght/12)*(capacitydecr)*df[["c_i"]][df[["c_i"]] > 0])
  df




  #now with 9 months (alternative simulation)

  #2 month plus 9 closure with a 20% capacity decrease

  capacitydecr <- 0.2 #capacity decrease
  clslenght <- 9 #number of months of closure

  #non-closed activities (c_i = 0)
  df[["wage2l9c20"]][df[["c_i"]] == 0] <- df[["wage2l"]][df[["c_i"]] == 0] #same wage as 2 month of lockdown


  #closed activities (c_i > 0)
  df[["wage2l9c20"]][df[["c_i"]] > 0] <- df[["wage2l"]][df[["c_i"]] > 0] - ( df[["wage"]][df[["c_i"]] > 0]*(clslenght/12)*(capacitydecr)*df[["c_i"]][df[["c_i"]] > 0])
  df

  #2 month plus 9 closure with a 30% capacity decrease

  capacitydecr <- 0.3 #capacity decrease
  clslenght <- 9 #number of months of closure

  #non-closed activities (c_i = 0)
  df[["wage2l9c30"]][df[["c_i"]] == 0] <- df[["wage2l"]][df[["c_i"]] == 0] #same wage as 2 month of lockdown


  #closed activities (c_i > 0)
  df[["wage2l9c30"]][df[["c_i"]] > 0] <- df[["wage2l"]][df[["c_i"]] > 0] - ( df[["wage"]][df[["c_i"]] > 0]*(clslenght/12)*(capacitydecr)*df[["c_i"]][df[["c_i"]] > 0])
  df



  #2 month plus 9 closure with a 40% capacity decrease

  capacitydecr <- 0.4 #capacity decrease
  clslenght <- 9 #number of months of closure

  #non-closed activities (c_i = 0)
  df[["wage2l9c40"]][df[["c_i"]] == 0] <- df[["wage2l"]][df[["c_i"]] == 0] #same wage as 2 month of lockdown

  #closed activities (c_i > 0)
  df[["wage2l9c40"]][df[["c_i"]] > 0] <- df[["wage2l"]][df[["c_i"]] > 0] - ( df[["wage"]][df[["c_i"]] > 0]*(clslenght/12)*(capacitydecr)*df[["c_i"]][df[["c_i"]] > 0])
  df

})


saveRDS(countries, file = "countries_3.rds") #saves file in current directory


