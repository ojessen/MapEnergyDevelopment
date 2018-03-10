library(maps)
library(ggthemes)
library(openxlsx)
library(gganimate)
library(dplyr)
library(tibble)
library(rworldmap)
library(ggplot2)
library(stringr)
library(tidyr)
options(stringsAsFactors = FALSE)

map.world <- map_data(map="world")

df_energy = read.xlsx(xlsxFile = "API_EG.ELC.ACCS.ZS_DS2_en_excel_v2.xlsx", sheet = "Data", startRow = 3) %>%
  as.tibble()

dist_regions = map.world %>% select(region) %>% distinct %>% unlist

df_world2 = data.frame(region = dist_regions, sov = NA)

for(idx in seq_along(df_world2$region)){
  cat(idx)
  tmp = try(sov.expand(df_world2$region[idx], FALSE))
  if(length(tmp) > 0){
    df_world2$sov[idx] = tmp  
  }
}

map_replacements = data.frame(region = c("Dominican Republic",
                                         "Guinea-Bissau",
                                         "Nigeria"),
                              new_iso = c("DO",
                                          "GW",
                                          "NG"))

iso_map_world = df_world2 %>%
  filter(!is.na(sov)) %>%
  mutate(ISO_A2 = iso.alpha(region, n = 2)) %>%
  left_join(map_replacements) %>%
  mutate(ISO_A2 = ifelse(is.na(new_iso), ISO_A2, new_iso)) 


iso_map_world %>% group_by(ISO_A2) %>% summarise(n = n()) %>% filter(n >1 )  %>%
  left_join(iso_map_world)

map_countries = df_energy %>% select(Country.Name, Country.Code) %>%
  mutate(ISO_A2 = purrr::map_chr(Country.Code, isoToName, nameColumn = "ISO_A2")) %>%
  left_join()

tmp = map_countries %>% filter(is.na(region)) %>% as.data.frame() %>%
  inner_join(iso_map_world, by = c("Country.Name" = "region"))

map_countries = map_countries %>%
  mutate(region = ifelse(Country.Name %in% tmp$Country.Name, Country.Name, region))

map_countries %>% filter(is.na(region)) %>% as.data.frame()

distinct_years = 1990:2014

df_energy_long = df_energy %>%
  select(-Country.Name, -Indicator.Name, -Indicator.Code) %>%
  gather(year, perc_energy, -Country.Code) %>%
  mutate(year = as.integer(year)) %>%
  filter(year %in% distinct_years) %>%
  left_join(map_countries %>% select(Country.Code, region)) %>%
  filter(!is.na(region)) %>%
  mutate(perc_energy_group = cut(perc_energy, breaks = c(0,25,50,75,90,99.9,100), include.lowest = TRUE))


df_energy_long %>% filter(str_detect(region, "Congo"))

#Add the data you want to map countries by to map.world
#In this example, I add lengths of country names plus some offset

map.world.expanded = data.frame()

map.world$year = NA

for(yr in distinct_years){
  tmp = map.world
  tmp$year = yr
  map.world.expanded = bind_rows(map.world.expanded, tmp)
}

df_energy_world = map.world.expanded %>%
  left_join(df_energy_long, by = c("region", "year")) %>%
  as.tibble

df_energy_world %>% filter(str_detect(region, "Congo"))

df_energy_world %>% filter( Country.Code == "COD", year == 1990)


df_filtered = df_energy_world %>%
  filter(year == 1990)

gg_2010 <- ggplot(data=df_filtered) +
  geom_map(map=map.world, aes(map_id=region, x=long, y=lat, fill=perc_energy, frame = year)) +
  scale_fill_gradient(low = "black", high = "green", guide = "colourbar") +
  coord_equal()+
  labs(fill = 'Perc Energy')
gg_2010

gg_2010 <- ggplot(data=df_filtered) +
  geom_map(map=map.world, aes(map_id=region, x=long, y=lat, fill=perc_energy_group, frame = year)) +
  coord_equal()+
  scale_fill_manual(values = c("[0,25]" = "black",
                      "(25,50]" = "red", 
                       "(50,75]" = "orange", 
                       "(75,90]" = "yellow", 
                      "(90,99.9]" = "lightgreen",
                      "(99.9,100]" = "darkgreen"), na.value = "grey") +
  labs(fill = 'Perc Energy')
gg_2010

     

cut(df_energy_long$perc_energy, breaks = c(0,25,50,75,90,99.9,100), include.lowest = TRUE)

gg <- ggplot(data=df_energy_world) +
  geom_map(map=map.world, aes(map_id=region, x=long, y=lat, fill=perc_energy_group, frame = year)) +
  scale_fill_manual(values = c("[0,25]" = "black",
                               "(25,50]" = "red", 
                               "(50,75]" = "orange", 
                               "(75,90]" = "yellow", 
                               "(90,99.9]" = "lightgreen",
                               "(99.9,100]" = "darkgreen"), na.value = "grey") +
  coord_equal()+
  labs(fill = 'Perc Energy')
gg


gganimate(gg, interval = .4, filename = "WorldEnergyDevelopment.gif", ani.width=1200, ani.height=800)
