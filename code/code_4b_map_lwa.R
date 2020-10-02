



indexeurope <- read_csv("table_1_descriptives_countries_grouped_lwa_prs.csv") #loads results from step 4a


names(indexeurope)[names(indexeurope) == "meanlwaall"] <- "lwa" #rename the mean lwa variable for each country
names(indexeurope)[names(indexeurope) == "countriesnames"] <- "country" #rename the mean lwa variable for each country


#function to align legend in graph/map

align_legend <- function(p, hjust = 0.5)
{
  # extract legend
  g <- cowplot::plot_to_gtable(p)
  grobs <- g$grobs
  legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
  legend <- grobs[[legend_index]]

  # extract guides table
  guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")

  # there can be multiple guides within one legend box
  for (gi in guides_index) {
    guides <- legend$grobs[[gi]]

    # add extra column for spacing
    # guides$width[5] is the extra spacing from the end of the legend text
    # to the end of the legend title. If we instead distribute it by `hjust:(1-hjust)` on
    # both sides, we get an aligned legend
    spacing <- guides$width[5]
    guides <- gtable::gtable_add_cols(guides, hjust*spacing, 1)
    guides$widths[6] <- (1-hjust)*spacing
    title_index <- guides$layout$name == "title"
    guides$layout$l[title_index] <- 2

    # reconstruct guides and write back
    legend$grobs[[gi]] <- guides
  }

  # reconstruct legend and write back
  g$grobs[[legend_index]] <- legend
  g
}


#load empty map

europemap <- sf::st_read("CNTR_RG_60M_2016_4326.shp") #read maps shape file

levels(europemap$CNTR_ID)[levels(europemap$CNTR_ID)=="EL"] <- "GR" #renames Greece code
class(europemap)


#make the map a manageable df object
europemap.f <- fortify(europemap, region = "CNTR_ID") #make it a matrix
class(europemap.f)
head(europemap.f)

#merge map with coefficients and reorder
merge.shp.coef<-merge(europemap.f, indexeurope, by.x="CNTR_ID", by.y= "country", all.x = F)
eurplotdata<-merge.shp.coef


## plots Europe

europlot <- ggplot() +
  geom_sf(data = eurplotdata, aes(group= CNTR_ID, fill = lwa), color = "black", size = 0.25) +
  scale_fill_distiller(name="LWA Index", palette = "YlOrBr", breaks = pretty_breaks(n = 5),  direction=1)+
  theme_nothing()+
 theme(legend.position=c(0, 0.82),legend.title=element_text(size=10, vjust = 0.5), legend.text = element_text(size =9))+
  coord_sf(xlim = c(-15, 36), ylim = c(32, 74), expand = FALSE)+
  theme(legend.title.align=0.5)

europlot2 <- ggdraw(align_legend(europlot))

#saves figure 1 in current WD
ggsave(europlot2, file="figure_1_lwa_europe_flat.pdf", device = "pdf", scale = 1, width = 6, height = 5, units = ("in"), dpi = 400, limitsize = TRUE)
