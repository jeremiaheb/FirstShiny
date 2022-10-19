
plotSubregionDens <- function(df1, yr, topOcc_n) {
  
  prettyName <- function(x) {
    
    a <- case_when(x == "DRY TORT" ~ "Dry Tortugas",
                   x == "FLA KEYS" ~ "Florida Keys",
                   x == "SEFCRI"   ~ "Southeast Florida",
                   x == "STTSTJ"   ~ "St. Thomas & St. John",
                   x == "STX"      ~ "St. Croix",
                   x == "PRICO"    ~ "Puerto Rico",
                   x == "FGB"      ~ "Flower Gardens")
    
    return(a)
    
  }
  
  o <- getDomainOccurrence(x = df1, species = df1$taxonomic_data$SPECIES_CD, years = yr )
  o <- o %>%
    top_n(x = ., n = topOcc_n, wt = occurrence)
  
  a <- getDomainDensity(df1, species = o$SPECIES_CD, years = yr)
  
  
  d <- a %>%
    mutate(SE = sqrt(var), denhect = density * 56.8, REGION = prettyName(REGION)) %>%
    mutate(hectSE = SE * 56.8) %>%
    merge(., select(df1$taxonomic_data, c("SPECIES_CD", "COMNAME")))
  
  
  scaleFun <- function(x) ifelse(x<1, sprintf("%.1f", x), sprintf("%.f", x))
  
  e <- ggplot(data = d, aes(x = denhect, y = reorder(COMNAME, denhect))) + 
    geom_point(size = 3) + 
    facet_grid(. ~ REGION) +
    scale_x_continuous(trans = "log10", labels = scaleFun) + 
    theme_Publication(base_size = 20) +
    theme(axis.text.y = element_text(size = 10),
          axis.title.y = element_blank()) +
    xlab("Density (ind ha-1)")
  
  
  return(e)
  
}