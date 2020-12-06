library(tidyverse)
library(data.table)
library(ggplot2)
library(ggrepel)
library(maps)
library(ggthemes)
library(mapproj)


url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_tbl <- fread(url)

world <- map_data("world")
covid_data_tbl <- covid_data_tbl%>% 
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  ))

join_tbl <- merge(world,covid_data_tbl,by.x= "region",by.y = "countriesAndTerritories")


join2_tbl <- join_tbl%>%
  select(region,deaths,cases,popData2019)

join2_tbl<-dplyr::group_by(join2_tbl, region) %>% dplyr::summarise_all(sum)
join3_tbl<- merge(world,join2_tbl, by.x= "region")

mortalityRate <- join3_tbl$deaths/join3_tbl$popData2019
#zVar <- (myVar - mean(myVar)) / sd(myVar)

join3_tbl%>% ggplot() + 
  geom_map(aes(map_id = region, fill = mortalityRate), map =world)  +
  expand_limits(x= join3_tbl$long, y =join3_tbl$lat)+ theme_map()+
  labs(title= "Confirmed COVID_19 deaths relative to the siye of the population
             More than 1.2 million confirmed COVID-19 deaths worldwide") +
  scale_fill_continuous(low='white', high= 'red') +theme_dark() +theme(legend.background = element_blank()) 
#scale_fill_viridis_c(option = "crimson") + theme_map() + theme(legend.background = element_blank())

