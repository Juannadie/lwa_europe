

#We load the data file from step 3

countries <- readRDS(file = "countries_3.rds")


#mean general function
wmeanf <- function(df, var, wg) {
  mean <- weighted.mean(df[[var]], df[[wg]], na.rm = T)
  mean
}

#get average tele

meantele <- sapply (countries, function(X) wmeanf (df = X, "teleworking", "weight"))

#get average lwa

meanlwa <- sapply (countries, function(X) wmeanf (df = X, "lwa", "weight"))


#Essentiality
#get average ei

meanei <- sapply (countries, function(X) wmeanf (df = X, "e_i", "weight"))

#Closed risk activities
#get average ci

meanci <- sapply (countries, function(X) wmeanf (df = X, "c_i", "weight"))

#dataframe
averages <- as.data.frame(as.matrix(cbind (meanci, meanei, meantele, meanlwa)))

#countries names
countriesnames <- names(countries)
averages$country <- countriesnames


averages$Region <- 'Central and Northern Europe'
averages$Region[averages$country %in% c("BG", "CZ", "EE", "HR", "HU", "LT", "LV", "PL", "RO", "SI", "SK")] <- 'Eastern Europe'
averages$Region[averages$country %in% c("CY", "ES", "GR", "IT", "PT")] <- 'Southern Europe'

write_csv(averages, "averages_ei_ci_tw_lwa_wage.csv") #optionally save the averages


#plots

#now with essentiality

averages$country <-reorder(averages$country, averages$meanei)

graph1 <- ggplot(data=averages, aes(x=country, y=meanei, fill=Region)) +
  theme_minimal()+
  geom_bar(stat="identity", width = 0.75) +
  ylab("Average Essentiality") +
  #xlab("Countries") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(axis.title.x=element_blank())+
  theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=1, margin=margin(0.5,0.5,0.5,0.5), size = 9))+
  theme(legend.position="bottom",legend.title=element_text(size=11, hjust = 0.5, vjust = 0.5), legend.text = element_text(size = 10, hjust = 0.5))+
  scale_y_continuous(limits = c(0, 0.52))

ggsave("figure2a_meanessential-bycountry.pdf", graph1, width = 9, height = 4, units = "in", dpi = 300 ) #saves figure 2a


#now with closure (graph 2b)

averages$country <-reorder(averages$country, averages$meanci)

graph2 <- ggplot(data=averages, aes(x=country, y=meanci, fill=Region)) +
  theme_minimal()+
  geom_bar(stat="identity", width = 0.75) +
  ylab("Average Closure") +
  #xlab("Countries") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(axis.title.x=element_blank())+
  theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=1, margin=margin(0.5,0.5,0.5,0.5), size = 9))+
  theme(legend.position="bottom",legend.title=element_text(size=11, hjust = 0.5, vjust = 0.5), legend.text = element_text(size = 10, hjust = 0.5))+
  scale_y_continuous(limits = c(0, 0.52))

ggsave("figure2b_meanclosure-bycountry.pdf", graph2, width = 9, height = 4, units = "in", dpi = 300 ) #saves figure 2b


# teleworking ; graph 2c

averages$country <-reorder(averages$country, averages$meantele)

graph3 <- ggplot(data=averages, aes(x=country, y=meantele, fill=Region)) +
  theme_minimal()+
  geom_bar(stat="identity", width = 0.75) +
  ylab("Average Teleworking") +
  #xlab("Countries") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(axis.title.x=element_blank())+
  theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=1, margin=margin(0.5,0.5,0.5,0.5), size = 9))+
  theme(legend.position="bottom",legend.title=element_text(size=11, hjust = 0.5, vjust = 0.5), legend.text = element_text(size = 10, hjust = 0.5))+
  scale_y_continuous(limits = c(0, 0.52))

ggsave("figure2c_meantele-bycountry.pdf", graph3, width = 9, height = 4, units = "in", dpi = 300 ) #saves figure 2c
