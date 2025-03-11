# create new figure displaying sst for map
library(httr)
library(ggplot2)
library(dplyr)
#library(akmarineareas2)
#saveRDS(ak, "Data/akmap.RDS")
#saveRDS(esr, "Data/esrmap.RDS")
library(sf)
library(lubridate)


dailysst <- httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/latest_sst_ecosystem'), type = "application/json") %>% 
  bind_rows() %>%
  rename_with(tolower) %>%
  mutate(lon360=ifelse(longitude>0, longitude, longitude+360),
         date=as_date(read_date))

dailysst <- dailysst %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326, agr = 'constant')%>%
  st_transform(crs=3338)

plotdate <-max(dailysst$date)

ak<-readRDS("Data/akmap.RDS")
esr <- readRDS("Data/esrmap.RDS")

png(filename="Figures/dailysstmap.png", width=1500, height=750)
ggplot()+
  geom_sf(data=dailysst, mapping=aes(color=temp),size=1)+
  geom_sf(data=ak)+
  geom_sf(data=esr, fill = NA)+
  scale_color_viridis_c()+
  scale_x_continuous(breaks=seq(0,360,10))+
  ggtitle(paste0("Alaskan sea surface temperature on ",plotdate))+
  theme_bw()
dev.off()
