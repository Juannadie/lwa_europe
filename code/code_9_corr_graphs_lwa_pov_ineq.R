
#We load the file

countries <- readRDS(file = "countries_3.rds")

giniresults <- read.csv("table3_prs_gini_index_increase_sd_eer_boot_6m.csv")

povertyresults <- read.csv("table2_prs_povwloss_sd_boot_6m.csv")

#get some functions

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

#get average lwa

meanlwa <- sapply (countries, function(X) wmeanf (df = X, "lwa", "weight"))

#get gini lwa

ginilwa <- sapply (countries, function(X) wginif (df = X, "lwa", "weight"))


#let's connect wage loss and lwa

dt <- as.data.frame(names(countries))
dt$povloss2l <- povertyresults$povgrowth2l
dt$povloss2l6c40 <- povertyresults$povgrowth2l6c40
dt$headinc2l <- povertyresults$headcountchange2l
dt$headinc2l6c40 <- povertyresults$headcountchange2l6c40
dt$giniinc2l <- giniresults$Increase.2l
dt$giniinc2lpc <- giniresults$Increase.2l.pc
dt$giniinc2l6c40 <- giniresults$Increase.2l6c40
dt$giniinc2l6c40pc <- giniresults$Increase.2l6c40.pc

dt$meanlwa <- meanlwa
dt$ginilwa <- ginilwa


names(dt)[1] <- c("country")


#some relations
#simple graph

lm_eqn = function(m) {

  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3, nsmall = 3));

  eq <- substitute(~~italic(r)^2~"="~r2,l);

  as.character(as.expression(eq));
}

#### absolute change 2l and mean lwa

graph <- ggplot(dt, aes(x=meanlwa,y=giniinc2l )) +
  geom_point(size=3, colour = "blue3")+
  ylab("Absolute increase in Gini (2m)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text_repel (aes(label=country), size = 3.5) +
  #geom_smooth(method = "lm", se = FALSE, size = 0.25, ) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 0.5, color = "red2", se = F)+
  scale_y_continuous(labels = scales::percent_format(suffix = ""))+
  xlab("Average LWA")+
  #scale_y_continuous(limits = c(0.5, 0.75)) +
  #scale_x_continuous(limits = c(15000, 27000))+
  theme(plot.margin=unit(c(0.75, 0.75, 0.75 , 0.75), "cm"))

graph

meanlwaabsincreasel2gini <- graph+ annotate ("text", y = 0.008, x = 0.4, label = lm_eqn(lm(giniinc2l ~ ((meanlwa) + I(meanlwa^2)) , dt)), color = "red3", size=4, parse = TRUE)


#### Now with GINI

#### absolute change 2l and mean lwa

graph <- ggplot(dt, aes(x=ginilwa,y=giniinc2l )) +
  geom_point(size=3, colour = "blue3")+
  ylab("Absolute increase in Gini (2m)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text_repel (aes(label=country), size = 3.5) +
  #geom_smooth(method = "lm", se = FALSE, size = 0.25, ) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 0.5, color = "red2", se = F)+
  scale_y_continuous(labels = scales::percent_format(suffix = ""))+
  xlab("Gini LWA")+
  #scale_y_continuous(limits = c(0.5, 0.75)) +
  #scale_x_continuous(limits = c(15000, 27000))+
  theme(plot.margin=unit(c(0.75, 0.75, 0.75 , 0.75), "cm"))

graph

ginilwaabsincreasel2gini <- graph+ annotate ("text", y = 0.008, x = 0.54, label = lm_eqn(lm(giniinc2l ~ ((ginilwa) + I(ginilwa^2)) , dt)), color = "red3", size=4, parse = TRUE)


  #NOW WITH POVERTY

#### absolute change 2l and mean lwa

graph <- ggplot(dt, aes(x=meanlwa,y=headinc2l )) +
  geom_point(size=3, colour = "blue3")+
  ylab("Increase in Headcount Poverty Index (pp) (2m)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text_repel (aes(label=country), size = 3.5) +
  #geom_smooth(method = "lm", se = FALSE, size = 0.25, ) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 0.5, color = "red2", se = F)+
  xlab("Average LWA")+
  scale_y_continuous(labels = scales::percent_format(suffix = ""))+
  #scale_y_continuous(limits = c(0.5, 0.75)) +
  #scale_x_continuous(limits = c(15000, 27000))+
  theme(plot.margin=unit(c(0.75, 0.75, 0.75 , 0.75), "cm"))

graph

meanlwaincreasel2headcount <- graph+ annotate ("text", y = 0.03, x = 0.4, label = lm_eqn(lm(headinc2l ~ ((meanlwa) + I(meanlwa^2)) , dt)), color = "red3", size=4, parse = TRUE)


#### Now with GINI

#### absolute change 2l and gini lwa

graph <- ggplot(dt, aes(x=ginilwa,y=headinc2l )) +
  geom_point(size=3, colour = "blue3")+
  ylab("Increase in Headcount Poverty Index (pp) (2m)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text_repel (aes(label=country), size = 3.5) +
  #geom_smooth(method = "lm", se = FALSE, size = 0.25, ) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 0.5, color = "red2", se = F)+
  xlab("Gini LWA")+
  #scale_y_continuous(limits = c(0.5, 0.75)) +
  #scale_x_continuous(limits = c(15000, 27000))+
  scale_y_continuous(labels = scales::percent_format(suffix = ""))+
  theme(plot.margin=unit(c(0.75, 0.75, 0.75 , 0.75), "cm"))

graph

ginilwaheadcountincl2<- graph+ annotate ("text", y = 0.035, x = 0.55, label = lm_eqn(lm(headinc2l ~ ((ginilwa) + I(ginilwa^2)) , dt)), color = "red3", size=4, parse = TRUE)


#4 graphs - chart

graphs4 <- ggarrange(meanlwaincreasel2headcount,  ginilwaheadcountincl2, meanlwaabsincreasel2gini, ginilwaabsincreasel2gini, labels = c("a)", "b)", "c)", "d)"), ncol = 2, nrow = 2)

#save Figure 4
ggsave("figure4_prs_cuadrante_l2_lwa_ineq_poverty.pdf", graphs4, width = 12, height = 10, units = "in", dpi = 400 )


##### NOW WITH THE 2M lockdwon + CL 3 ######

#### absolute change 2l and mean lwa

graph <- ggplot(dt, aes(x=meanlwa,y=giniinc2l6c40 )) +
  geom_point(size=3, colour = "blue3")+
  ylab("Absolute increase in Gini (2m+CL3)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text_repel (aes(label=country), size = 3.5) +
  #geom_smooth(method = "lm", se = FALSE, size = 0.25, ) +
  stat_smooth(method = "lm",formula = y ~ x + I(x^2), size = 0.5, color = "red2", se = F)+
  scale_y_continuous(labels = scales::percent_format(suffix = ""))+
  xlab("Average LWA")+
  #scale_y_continuous(limits = c(0.5, 0.75)) +
  #scale_x_continuous(limits = c(15000, 27000))+
  theme(plot.margin=unit(c(0.75, 0.75, 0.75 , 0.75), "cm"))

graph

meanlwaabsincrease2l6c40gini <- graph+ annotate ("text", y = 0.018, x = 0.4, label = lm_eqn(lm(giniinc2l6c40 ~ ((meanlwa) + I(meanlwa^2)) , dt)), color = "red3", size=4, parse = TRUE)


#### absolute change 2l and gini lwa

graph <- ggplot(dt, aes(x=ginilwa,y=giniinc2l6c40 )) +
  geom_point(size=3, colour = "blue3")+
  ylab("Absolute increase in Gini (2m+CL3)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text_repel (aes(label=country), size = 3.5) +
  #geom_smooth(method = "lm", se = FALSE, size = 0.25, ) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 0.5, color = "red2", se = F)+
  xlab("Gini LWA")+
  scale_y_continuous(labels = scales::percent_format(suffix = ""))+
  #scale_y_continuous(limits = c(0.5, 0.75)) +
  #scale_x_continuous(limits = c(15000, 27000))+
  theme(plot.margin=unit(c(0.75, 0.75, 0.75 , 0.75), "cm"))

graph

ginilwaabsincrease2l6c40gini <- graph+ annotate ("text", y = 0.037, x = 0.4, label = lm_eqn(lm(giniinc2l6c40 ~ ((ginilwa) + I(ginilwa^2)) , dt)), color = "red3", size=4, parse = TRUE)


#NOW WITH POVERTY

#### absolute change 2l and mean lwa

graph <- ggplot(dt, aes(x=meanlwa,y=headinc2l6c40 )) +
  geom_point(size=3, colour = "blue3")+
  ylab("Increase in Headcount Poverty Index (pp) (2m+CL3)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text_repel (aes(label=country), size = 3.5) +
  #geom_smooth(method = "lm", se = FALSE, size = 0.25, ) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 0.5, color = "red2", se = F)+
  scale_y_continuous(labels = scales::percent_format(suffix = ""))+
  xlab("Average LWA")+
  theme(plot.margin=unit(c(0.75, 0.75, 0.75 , 0.75), "cm"))

graph

meanlwaincrease2l6c40headcount <- graph+ annotate ("text", y = 0.06, x = 0.4, label = lm_eqn(lm(headinc2l6c40 ~ ((meanlwa) + I(meanlwa^2)) , dt)), color = "red3", size=4, parse = TRUE)


#### absolute change 2l and gini lwa

graph <- ggplot(dt, aes(x=ginilwa,y=headinc2l6c40 )) +
  geom_point(size=3, colour = "blue3")+
  ylab("Increase in Headcount Poverty Index (pp) (2m+CL3)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text_repel (aes(label=country), size = 3.5) +
  #geom_smooth(method = "lm", se = FALSE, size = 0.25, ) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 0.5, color = "red2", se = F)+
  xlab("Gini LWA")+
  #scale_y_continuous(limits = c(0.5, 0.75)) +
  #scale_x_continuous(limits = c(15000, 27000))+
  scale_y_continuous(labels = scales::percent_format(suffix = ""))+
  theme(plot.margin=unit(c(0.75, 0.75, 0.75 , 0.75), "cm"))

graph

ginilwaheadcountinc2l6c40 <- graph+ annotate ("text", y = 0.15, x = 0.4, label = lm_eqn(lm(headinc2l6c40 ~ ((ginilwa) + I(ginilwa^2)) , dt)), color = "red3", size=4, parse = TRUE)


graphs4bis <- ggarrange(meanlwaincrease2l6c40headcount,  ginilwaheadcountinc2l6c40, meanlwaabsincrease2l6c40gini, ginilwaabsincrease2l6c40gini, labels = c("a)", "b)", "c)", "d)"), ncol = 2, nrow = 2)


#Save Figure C1 (Appendix C)

ggsave("figureC1_prs_cuadrante_2l6c40_lwa_ineq_poverty.pdf", graphs4bis, width = 12, height = 10, units = "in", dpi = 400)
