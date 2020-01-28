pacman::p_load(readxl, dplyr, sf, ggplot2, GISTools, viridis)

d = read_excel("data/dph_wq_Death_Rate__by_Residence__Drug_Overdoses_1_22_20_05_4_32_159265_PM.xlsx", skip = 2) %>% 
  slice(1:159) %>% 
  `colnames<-`(c("county_name", "death_rate")) %>% 
  mutate(death_rate = as.numeric(death_rate))

#g = st_as_sf(georgia)
#write_sf(g, "data/Georgia.shp")
go = read_sf("data/Georgia.shp") %>% 
  left_join(d, by = c("Name" = "county_name"))

go %>% 
  ggplot() + 
  geom_sf(aes(fill = death_rate), color = "grey5") + 
  scale_fill_viridis(option = "magma") + 
  theme_void()
