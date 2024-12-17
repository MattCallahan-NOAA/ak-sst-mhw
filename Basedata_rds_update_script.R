require(httr)
require(dplyr)
require(lubridate)



#  Define the Bering Sea dataset
base_update <- httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/ecosystem_sub_crw_avg_sst?start_date=19850101&end_date=20501130'), type = "application/json") %>% 
  bind_rows %>% 
  mutate(date=as_date(READ_DATE)) %>% 
  data.frame %>% 
  dplyr::select(date,meansst=MEANSST,Ecosystem_sub=ECOSYSTEM_SUB)%>%
  saveRDS("Data/base.rds")



  