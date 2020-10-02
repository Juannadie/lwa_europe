library(dplyr) #necessary for function bind_rows


#We load the file from step 3

countries <- readRDS(file = "countries_3.rds")


#Function to split the sample in weighted centiles

centilew <- function (vector, w, decreasing = FALSE)
{
  if (class(vector) != "integer" && class(vector) != "numeric") {
    stop("Invalid input: vector should be either integer or numeric")
  }
  else if (class(decreasing) != "logical" | length(decreasing) !=
           1) {
    stop("Invalid input: decreasing should be a logical vector of length 1")
  }
  else {

    vector = vector + seq_along(vector+runif(1,1e-9,1e-6)) # add very small random number to get unique cuts
    breaks = reldist::wtd.quantile(vector, q = seq(0, 1, by = 0.01), na.rm = T, weight = w)
    ventile <- as.integer(as.character(cut(vector, breaks, labels = if (decreasing == FALSE) { 1:100
  } else {100:1}, include.lowest = TRUE)))
    return(ventile)
  }
}

#We apply that function to the wage distribution, creating numbered (increasing) centiles

countries <- lapply(countries, function(X) {
  X[["centilew"]]<- centilew(X[["wage"]], X[["weight"]])
  X
})


## we calculate the percentage wage loss for each scenario, although we will only print in a graph the first scenario

countries <- lapply(countries, function(X) {

  X[["pcloss2l"]] <- ((X[["wage2l"]] - X[["wage"]])/X[["wage"]])*100
  X
})


countries <- lapply(countries, function(X) {

  X[["pcloss2l6c20"]] <- ((X[["wage2l6c20"]] - X[["wage"]])/X[["wage"]])*100
  X
})

countries <- lapply(countries, function(X) {

  X[["pcloss2l6c30"]] <- ((X[["wage2l6c30"]] - X[["wage"]])/X[["wage"]])*100
  X
})

countries <- lapply(countries, function(X) {

  X[["pcloss2l6c40"]] <- ((X[["wage2l6c40"]] - X[["wage"]])/X[["wage"]])*100
  X
})


#We now calculate the percentage loss at each centile
#First scenario (2m lockdown)
 pctloss2l <- lapply(countries, function(X) {
    aa <- as.data.frame(as.matrix(sapply(split(X, X[["centilew"]]), function(X) weighted.mean(X[["pcloss2l"]], w = X[["weight"]]))))
    aa["country"] <- sample(X[["country"]], size = 1)
    aa
  })

#We add a numbering for the percentiles in the new data frame
 pctloss2l <- lapply(pctloss2l, function(X) {
   X[["ptc"]]<- seq(1,100,1)
   X
 })

 pctlossdf <- bind_rows(pctloss2l) #convert the list to data frame

 names(pctlossdf) <- c("loss2l", "country", "pct") #name the three variabless


 #A second scenario (2l 6m30) #not printed but could be used to print incidence curves of a the second scenario

 pctloss2l6c30 <- lapply(countries, function(X) {
   aa <- as.data.frame(as.matrix(sapply(split(X, X[["centilew"]]), function(X) weighted.mean(X[["pcloss2l6c30"]], w = X[["weight"]]))))
   aa["country"] <- sample(X[["country"]], size = 1)
   aa
 })


 pctloss2l6c30 <- lapply(pctloss2l6c30, function(X) {
   X[["pct"]]<- seq(1,100,1)
   X
 })

pctloss2l6c30df <- bind_rows( pctloss2l6c30 )

names(pctloss2l6c30df) <- c("loss2l6c30", "country", "pct")

#We paste the second measure into the first dataframe

pctlossdf$loss2l6c30 <- pctloss2l6c30df$loss2l6c30

names(countries)

#We only print the first scenario (2m lockdown)

#graph main big countries

g1 <- ggplot(pctlossdf[pctlossdf$country %in% c("UK",  "FR", "DE"),], aes()) +
  geom_smooth(size=1, span = 0.5, aes(x=pct, y=loss2l, colour=country, shape=country), se = F)+
  xlab("Percentile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +

  ylab("Wage Loss (%)") +
  scale_y_continuous(limits = c(-15, 2)) +
  theme_minimal()+
  theme(legend.title = element_blank())+
  theme(plot.margin=unit(c(0.5, 0.5, 0.5 , 0.5), "cm"))

ggsave("figure3_right_lic_DE_FR_UK.pdf", g1, width = 7, height = 5, units = "in", dpi = 300 ) #save figure 3 right side


g2 <- ggplot(pctlossdf[pctlossdf$country %in% c("CY",  "RO"),], aes()) +
  geom_smooth(size=1, span = 0.5, aes(x=pct, y=loss2l, colour=country, shape=country), se = F)+
  #geom_smooth(size=1.25, aes(x=pct, y=loss2l, colour=country))+
  xlab("Percentile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Wage Loss (%)")+
  scale_y_continuous(limits = c(-15, 2)) +
  theme_minimal()+
  theme(legend.title = element_blank())+
  theme(plot.margin=unit(c(0.5, 0.5, 0.5 , 0.5), "cm"))

ggsave("figure3_left_lic_CY_RO.pdf", g2, width = 7, height = 5, units = "in", dpi = 300) #save figure 3 left side

