library(shiny)
library(tidyverse)
library(shinycssloaders)
library(lubridate)
library(cowplot)
library(httr)
library(heatwaveR)
library(gridExtra)
library(scales)
#library(magick)

ui <- fluidPage(
  

  #Define Regional output in separate tabs
  tabsetPanel(type="tabs", 
              tabPanel("Map",
              tags$blockquote("We present daily sea surface temperatures and marine heatwave status for each of the ecosystem regions
 managed by the Alaska Fisheries Science Center. Temperatures are updated automatically
  using satellite data curated by NOAA's Coral Reef Watch Program (https://coralreefwatch.noaa.gov/).
  The current year's daily temperatures (black lines) are compared to the previous year (blue line),
  the daily average (1985-2014), and each of the individual years since 1985 (grey lines)."),
 tags$blockquote("Marine heatwave calculations are performed on the daily SST data using the heatwaveR package (https://robwschlegel.github.io/heatwaveR/)."),
 tags$blockquote("More information can be found here (https://www.fisheries.noaa.gov/feature-story/current-sea-surface-temperatures-eastern-bering-sea),
 on github (https://github.com/MattCallahan-NOAA/SST-shiny/), or by contacting emily.lemagie@noaa.gov or matt.callahan@noaa.gov."),
            # tabPanel("2020 Bering Sea", textOutput("text"),
                        imageOutput(outputId = "ESR_Map")%>%
                          withSpinner()),
              tabPanel("Bering Sea",
                       downloadButton("BSpng", "Download BS image"),
                       downloadButton("BSData","Download BS SST Data"),
                       plotOutput(outputId = "BSplot", height = 1000, width=1200)%>%
                         withSpinner()),
              tabPanel("Gulf of Alaska", 
                       downloadButton("GOApng","Download GOA image"),
                       downloadButton("GOAData","Download GOA SST"),
                       plotOutput(outputId = "GOAplot", height = 1000, width=1200)%>%
                         withSpinner()),
              tabPanel("Aleutian Islands",
                       downloadButton("AIpng","Download AI image"),
                       downloadButton("AIData","Download AI SST"),
                       plotOutput(outputId = "AIplot", height = 1000, width=1200)%>%
                         withSpinner())),
)

server <- function(input, output) {
  ####--------------------------------------------------------------####
  #create first tab that will hopefully load before the other tabs
  output$ESR_Map<-renderImage({
    filename <- normalizePath(file.path('Figures/dailysstmap.png'))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         height = 800)
    
  }, deleteFile = FALSE)
  
  ####-----------------------------------------------------------------####
  #code relevent to all upper panels in subsequent plots
  
  #  Load 508 compliant NOAA colors
  OceansBlue1='#0093D0'
  OceansBlue2='#0055A4' # rebecca dark blue
  Crustacean1='#FF8300'
  UrchinPurple1='#7F7FFF'
  SeagrassGreen4='#D0D0D0' # This is just grey
  #  Assign colors to different time series.
  current.year.color <- "black"#CoralRed1 #OceansBlue1
  last.year.color <- OceansBlue1#WavesTeal1
  mean.color <- UrchinPurple1
  #  Set default plot theme
  theme_set(theme_cowplot())
  
  #  Specify legend position coordinates (top panel)
  mylegx <- 0.525
  mylegy <- 0.865
  
  #  Specify NOAA logo position coordinates (top panel)
  mylogox <- 0.045
  mylogoy <- 0.285
  
  ####-------------------------------------------------------------####
  #Load Base data
  base<-readRDS("Data/base.rds")
  
  #  Define the Bering Sea datase
  BSupdateddata<-base%>%
    filter(Ecosystem_sub %in% c("Southeastern Bering Sea", "Northern Bering Sea"))
  
  BSdata <- BSupdateddata %>%
    rename_all(tolower) %>% 
    mutate(read_date=date,
           esr_region=ecosystem_sub,
           month=month(read_date),
           day=day(read_date),
           year=year(read_date),
           newdate=as.Date(ifelse(month==12,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                  as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
           year2=ifelse(month==12,year+1,year)) %>% # To have our years go from Dec-Nov, force December to be part of the subsequent year.
    arrange(read_date) 
  #  Set year criteria to automatically identify the current and previous years
  current.year <- max(BSdata$year2)
  last.year <- current.year-1
  
  climatology_start_year <- 1985
  climatology_start_date <- "1985-01-01"
  climatology_end_year <- 2014
  climatology_end_date <- "2014-12-31"
  mean.years <- climatology_start_year:climatology_end_year 
  mean.lab <- paste0("Mean ",climatology_start_year,"-",climatology_end_year)

  
  ####---------------------------------------------------####
  #Plots for P1
  #  Create plotting function that will allow selection of 2 ESR regions
  BSplotfun <- function(region1,region2){
    mylines_base <- ggplot() +
      geom_line(data=BSdata %>% filter(year2<last.year & esr_region%in%(c(region1,region2))), # Older years are grey lines.
                aes(newdate,meansst,group=factor(year2),col='mygrey'),size=0.3) +
      geom_line(data=BSdata %>% filter(year2==last.year & esr_region%in%(c(region1,region2))), # The previous year
                aes(newdate,meansst,color='last.year.color'),size=0.75) +
      geom_line(data=BSdata %>% 
                  filter(year%in%mean.years & esr_region%in%(c(region1,region2))) %>% # The mean from 1986-2015
                  group_by(esr_region,newdate) %>% 
                  summarise(meantemp=mean(meansst,na.rm=TRUE)),
                aes(newdate,meantemp,col='mean.color'),size=0.65,linetype="solid") +
      geom_line(data=BSdata %>% filter(year2==current.year & esr_region%in%(c(region1,region2))), # This year
                aes(newdate,meansst,color='current.year.color'),size=0.75) +
      facet_wrap(~esr_region,ncol=2) + 
      scale_color_manual(name="",
                         breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                         values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                         labels=c(current.year,last.year,paste0('1985-',last.year-1),mean.lab)) +
      scale_linetype_manual(values=c("solid","solid","solid","dashed")) +
      ylab("Sea Surface Temperature (°C)") + 
      xlab("") +
      scale_x_date(date_breaks="1 month",
                   date_labels = "%b",
                   expand = c(0.025,0.025)) + 
      theme(legend.position=c(mylegx,mylegy),
            legend.text = element_text(size=20,family="sans"),
            legend.background = element_blank(),
            legend.title = element_blank(),
            strip.text = element_text(size=24,color="white",family="sans",face="bold"),
            strip.background = element_rect(fill=OceansBlue2),
            axis.title.y = element_text(size=20,family="sans"),
            axis.text.y = element_text(size=16,family="sans"),
            panel.border=element_rect(colour="black",size=0.75),
            axis.text.x=element_blank(),
            legend.key.size = unit(0.35,"cm"),
            plot.margin=unit(c(-0.1,0.05,0,0),"cm")) 
    
    ggdraw(mylines_base)
  }
  
  pb1 <-reactive(BSplotfun("Northern Bering Sea","Southeastern Bering Sea") + 
    draw_image("Figures/fisheries_header_logo_jul2019.png",scale=0.2,x=mylogox,y=mylogoy,hjust=0.35))
  
  ####-------------------------------------------------####
  #Code used for all lower panels
  
  #  Create custom categories for lines
  lineColCat <- c(
    "Temperature" = "black",
    "Baseline" = mean.color,
    "Moderate (1x Threshold)" = "gray60",
    "Strong (2x Threshold)" = "gray60",
    "Severe (3x Threshold)" = "gray60",
    "Extreme (4x Threshold)" = "gray60"
  )
  
  #  Create flame fill parameters
  fillColCat <- c(
    "Moderate" = "#ffc866",
    "Strong" = "#ff6900",
    "Severe" = "#9e0000",
    "Extreme" = "#2d0000"
  )
  
  #  Modified flame fill parameters
  Moderate = "#ffc866"
  Strong = "#ff6900"
  Severe = "#9e0000"
  Extreme = "#2d0000"
  
  #  Format plot (modified from theme_cowplot)
  mytheme <- theme(strip.text = element_text(size=24,color="white",family="sans",face="bold"),
                   strip.background = element_rect(fill=OceansBlue2),
                   axis.title = element_text(size=20,family="sans",color="black"),
                   axis.text = element_text(size=16,family="sans",color="black"),
                   panel.border=element_rect(colour="black",fill=NA,size=0.5),
                   panel.background = element_blank(),
                   plot.margin=unit(c(0.65,0,0.65,0),"cm"),
                   legend.position=c(0.6,0.7),
                   legend.background = element_blank(),
                   legend.key.size = unit(1,"line"))
  
  ####------------------------------------------------####
  #Bering Sea lower panel code

  # Use heatwaveR package to detect marine heatwaves.
  BSmhw <- (detect_event(ts2clm(BSupdateddata %>% 
                                  filter(Ecosystem_sub=="Southeastern Bering Sea") %>% 
                                  rename(t=date,temp=meansst) %>% 
                                  arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% 
    mutate(region=paste0(current.year," Southeastern Bering Sea Heatwaves")) %>% 
    bind_rows((detect_event(ts2clm(BSupdateddata %>%
                                     filter(Ecosystem_sub=="Northern Bering Sea") %>% 
                                     rename(t=date,temp=meansst) %>% 
                                     arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% 
                mutate(region=paste0(current.year," Northern Bering Sea Heatwaves")))
  
  #  Create a vector of the days remaining in the year without data.
  yearvec <- seq.Date(max(BSmhw$t)+1,as_date(paste0(current.year,"-11-30")),"day")
  #create year length vector
  current.year.length<-ifelse(leap_year(current.year)==FALSE, 365,366)

  #  Replace the current year with the previous year for our remaining days vector.
  BSdummydat <- data.frame(t=yearvec-current.year.length,newt=yearvec) %>% 
    inner_join(BSmhw %>% dplyr::select(region,thresh,seas,t)) %>% 
    dplyr::select(t=newt,region,thresh,seas) %>% 
    mutate(temp=NA)
  # Calculate threshold values for heatwave categories. This code directly from Schegel & Smit
  BSclim_cat <- BSmhw %>%
    bind_rows(BSdummydat) %>% 
    group_by(region) %>% 
    dplyr::mutate(diff = thresh - seas,
                  thresh_2x = thresh + diff,
                  thresh_3x = thresh_2x + diff,
                  thresh_4x = thresh_3x + diff,
                  year=year(t)) %>% 
    arrange(t)
  
  #  Create annotation text for plot
  BSmhw_lab <- data.frame(region=c(paste0(current.year," Northern Bering Sea Heatwaves"),paste0(current.year," Southeastern Bering Sea Heatwaves")),
                          t=c(as_date(paste0(last.year,"-12-05")),as_date(paste0(last.year,"-12-05"))),
                          temp=rev(c((1*max(BSclim_cat$temp,na.rm=TRUE)),(0.9*max(BSclim_cat$temp,na.rm=TRUE)))),
                          mylab=rev(c("Heatwave intensity increases\n(successive dotted lines)\nas waters warm.",
                                      "Heatwaves occur when daily\nSST exceeds the 90th\npercentile of normal\n(lowest dotted line) for\n5 consecutive days.")))
  #  Plotting code only slightly modified from heatwaveR vignette
  pb2 <- reactive(ggplot(data = BSclim_cat %>% filter(t>=as.Date(paste0(last.year,"-12-01"))), aes(x = t, y = temp)) +
    geom_line(aes(y = temp, col = "Temperature"), size = 0.85) +
    geom_flame(aes(y2 = thresh, fill = Moderate)) +
    geom_flame(aes(y2 = thresh_2x, fill = Strong)) +
    geom_flame(aes(y2 = thresh_3x, fill = Severe)) +
    geom_flame(aes(y2 = thresh_4x, fill = Extreme)) +
    geom_line(aes(y = thresh_2x, col = "Strong (2x Threshold)"), size = 0.5, linetype = "dotted") +
    geom_line(aes(y = thresh_3x, col = "Severe (3x Threshold)"), size = 0.5, linetype = "dotted") +
    geom_line(aes(y = thresh_4x, col = "Extreme (4x Threshold)"), size = 0.5, linetype = "dotted") +
    geom_line(aes(y = seas, col = "Baseline"), size = 0.65,linetype="solid") +
    geom_line(aes(y = thresh, col = "Moderate (1x Threshold)"), size = 0.5,linetype= "dotted") +
    
    geom_text(data=BSmhw_lab,aes(x=t,y=temp,label=mylab),hjust=0,size=8,family="sans",lineheight=1) +
    scale_colour_manual(name = NULL, values = lineColCat,
                        breaks = c("Temperature", "Baseline", "Moderate (1x Threshold)"),guide=FALSE) +
    scale_fill_manual(name = "Heatwave\nIntensity", values = c(Extreme,Severe,Strong,Moderate),labels=c("Extreme","Severe","Strong","Moderate")#, guide = FALSE
    ) +
    scale_x_date(limits=c(as_date(paste0(last.year,"-12-01")),as_date(paste0(current.year,"-11-30"))),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
    labs(y = "Sea Surface Temperature (°C)", x = NULL) + 
    facet_wrap(~region,ncol=2) +
    mytheme + 
    theme(strip.text=element_text(size=24),
          legend.position=c(0.815,0.285),
          legend.title = element_text(size=20),
          legend.key.size = unit(0.75,"line"),
          legend.text = element_text(size=16),
          axis.title.x=element_blank(),
          axis.text.x=element_text(color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
          plot.margin=unit(c(-0.7,0.05,3,0),"cm")))
  pb2b <- reactive(ggdraw(pb2()) + 
    annotate("text",x=0.5 ,y=0.065,label=paste0("NOAA Coral Reef Watch data, courtesy NOAA Pacific Islands Ocean Observing System (Updated: ",
                                                format(max(BSdata$date),"%m-%d-%Y"),
                                                ")\n Data are modeled satellite products and periodic discrepancies or gaps may exist across sensors and products.\n                                    Contact: matt.callahan@noaa.gov, Alaska Fisheries Science Center "),
             hjust=0.5, size=7,family="sans",fontface=1,color=OceansBlue2,lineheight=0.85) )
  
  ####-----------------------------------------------####
  #combine plots
  pb3<-reactive(plot_grid(pb1(),pb2b(),ncol=1))
  
  ####--------------------------------------------------------####
  #print to shiny
  #BS render plot
  output$BSplot <- renderPlot({
    pb3()
  })
  

  ####---------------------------------------------------####
  #Download buttons
  
  #image
  #image
  output$BSpng<-downloadHandler(
    filename = function() {
      paste("BS-SST-", Sys.Date(), ".png", sep="")
    },
    contentType = "image/png",
    content= function(file){
      png(file, height=14.25, width=18, units="in", res=300)
      print(pb3())
      dev.off()
    }
    
  )
  #Download buttons
  #csv
  output$BSData<-downloadHandler(
    filename = function() {
      paste("BS-SST-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(BSupdateddata, file)
    })
  
  ####----------------------------------------------------------####
  ###GOA tab
  #query webservice
  
  GOAupdateddata<-base%>%
    filter(Ecosystem_sub %in% c("Western Gulf of Alaska", "Eastern Gulf of Alaska"))
  
  GOAdata <- 
    GOAupdateddata %>% 
    rename_all(tolower) %>% 
    mutate(read_date=date,
           month=month(read_date),
           esr_region=factor(ecosystem_sub, levels=c("Western Gulf of Alaska","Eastern Gulf of Alaska")),
           day=day(read_date),
           year=year(read_date),
           newdate=as.Date(ifelse(month==12,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                  as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
           year2=ifelse(month==12,year+1,year)) %>% # To have our years go from Dec-Nov, force December to be part of the subsequent year.
    arrange(read_date) 
  ####---------------------------------------------------------------#####
  #  Create plotting function that will allow selection of 2 ESR regions
  GOAplotfun <- function(region1,region2){
    mylines_base <- ggplot() +
      geom_line(data=GOAdata %>% filter(year2<last.year & esr_region%in%(c(region1,region2))), # Older years are grey lines.
                aes(newdate,meansst,group=factor(year2),col='mygrey'),size=0.3) +
      geom_line(data=GOAdata %>% filter(year2==last.year & esr_region%in%(c(region1,region2))), # The previous year
                aes(newdate,meansst,color='last.year.color'),size=0.75) +
      geom_line(data=GOAdata %>% 
                  filter(year%in%mean.years & esr_region%in%(c(region1,region2))) %>% # The mean from 1986-2015
                  group_by(esr_region,newdate) %>% 
                  summarise(meantemp=mean(meansst,na.rm=TRUE)),
                aes(newdate,meantemp,col='mean.color'),size=0.65,linetype="solid") +
      geom_line(data=GOAdata %>% filter(year2==current.year & esr_region%in%(c(region1,region2))), # This year
                aes(newdate,meansst,color='current.year.color'),size=0.75) +
      facet_wrap(~esr_region,ncol=2) + 
      scale_color_manual(name="",
                         breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                         values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                         labels=c(current.year,last.year,paste0('1985-',last.year-1),mean.lab)) +
      scale_linetype_manual(values=c("solid","solid","solid","dashed")) +
      ylab("Sea Surface Temperature (°C)") + 
      xlab("") +
      scale_x_date(date_breaks="1 month",
                   date_labels = "%b",
                   expand = c(0.025,0.025)) + 
      theme(legend.position=c(mylegx,mylegy),
            legend.text = element_text(size=20,family="sans"),
            legend.background = element_blank(),
            legend.title = element_blank(),
            strip.text = element_text(size=24,color="white",family="sans",face="bold"),
            strip.background = element_rect(fill=OceansBlue2),
            axis.title.y = element_text(size=20,family="sans"),
            axis.text.y = element_text(size=16,family="sans"),
            panel.border=element_rect(colour="black",size=0.75),
            axis.text.x=element_blank(),
            legend.key.size = unit(0.35,"cm"),
            plot.margin=unit(c(-0.1,0.05,0,0),"cm")) 
    
    ggdraw(mylines_base)
  }
  
  pg1 <- reactive(GOAplotfun("Western Gulf of Alaska","Eastern Gulf of Alaska") +
    draw_image("Figures/fisheries_header_logo_jul2019.png",scale=0.2,x=mylogox,y=mylogoy,hjust=0.35))
  
  #Create bottom panel
  
  GOAmhw <- (detect_event(ts2clm(GOAupdateddata %>% 
                                   filter(Ecosystem_sub=="Western Gulf of Alaska") %>% 
                                   rename(t=date,temp=meansst) %>% 
                                   arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% 
    mutate(region=paste0(current.year," Western GOA Heatwaves")) %>% 
    bind_rows((detect_event(ts2clm(GOAupdateddata %>%
                                     filter(Ecosystem_sub=="Eastern Gulf of Alaska") %>% 
                                     rename(t=date,temp=meansst) %>% 
                                     arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% 
                mutate(region=paste0(current.year," Eastern GOA Heatwaves")))
  

  #  Replace the current year with the previous year for our remaining days vector.
  GOAdummydat <- data.frame(t=yearvec-current.year.length, newt=yearvec) %>% 
    inner_join(GOAmhw %>% dplyr::select(region,thresh,seas,t)) %>% 
    dplyr::select(t=newt,region,thresh,seas) %>% 
    mutate(temp=NA)
  
  # Calculate threshold values for heatwave categories. This code directly from Schegel & Smit
  GOAclim_cat <- GOAmhw %>%
    bind_rows(GOAdummydat) %>% 
    group_by(region) %>% 
    dplyr::mutate(diff = thresh - seas,
                  thresh_2x = thresh + diff,
                  thresh_3x = thresh_2x + diff,
                  thresh_4x = thresh_3x + diff,
                  year=year(t),
                  region2=factor(region, levels=c(paste0(current.year," Western GOA Heatwaves"),
                                                  paste0(current.year," Eastern GOA Heatwaves")))) %>% 
    arrange(t)
  

  
  #  Create annotation text for plot
  GOAmhw_lab <- data.frame(region2=factor(c(paste0(current.year," Western GOA Heatwaves"),paste0(current.year, " Eastern GOA Heatwaves"))),
                           t=c(as_date(paste0(last.year,"-12-05")),as_date(paste0(last.year,"-12-05"))),
                           temp=rev(c((1*max(GOAclim_cat$temp,na.rm=TRUE)),(0.9*max(GOAclim_cat$temp,na.rm=TRUE)))),
                           mylab=rev(c("Heatwave intensity increases\n(successive dotted lines)\nas waters warm.",
                                       "Heatwaves occur when daily\nSST exceeds the 90th\npercentile of normal\n(lowest dotted line) for\n5 consecutive days.")))
  
  
  
  pg2 <- reactive(ggplot(data = GOAclim_cat %>% filter(t>=as.Date(paste0(last.year,"-12-01"))), aes(x = t, y = temp)) +
    geom_line(aes(y = temp, col = "Temperature"), size = 0.85) +
    geom_flame(aes(y2 = thresh, fill = Moderate)) +
    geom_flame(aes(y2 = thresh_2x, fill = Strong)) +
    geom_flame(aes(y2 = thresh_3x, fill = Severe)) +
    geom_flame(aes(y2 = thresh_4x, fill = Extreme)) +
    geom_line(aes(y = thresh_2x, col = "Strong (2x Threshold)"), size = 0.5, linetype = "dotted") +
    geom_line(aes(y = thresh_3x, col = "Severe (3x Threshold)"), size = 0.5, linetype = "dotted") +
    geom_line(aes(y = thresh_4x, col = "Extreme (4x Threshold)"), size = 0.5, linetype = "dotted") +
    geom_line(aes(y = seas, col = "Baseline"), size = 0.65,linetype="solid") +
    geom_line(aes(y = thresh, col = "Moderate (1x Threshold)"), size = 0.5,linetype= "dotted") +
    
    geom_text(data=GOAmhw_lab,aes(x=t,y=temp,label=mylab),hjust=0,size=8,family="sans",lineheight=1) +
    scale_colour_manual(name = NULL, values = lineColCat,
                        breaks = c("Temperature", "Baseline", "Moderate (1x Threshold)"),guide=FALSE) +
    scale_fill_manual(name = "Heatwave\nIntensity", values = c(Extreme,Severe,Strong,Moderate),labels=c("Extreme","Severe","Strong","Moderate")#, guide = FALSE
    ) +
    scale_x_date(limits=c(as_date(paste0(last.year,"-12-01")),as_date(paste0(current.year,"-11-30"))),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
    labs(y = "Sea Surface Temperature (°C)", x = NULL) + 
    facet_wrap(~region2,ncol=2) +
    mytheme + 
    theme(strip.text=element_text(size=24),
          legend.position=c(0.815,0.285),
          legend.title = element_text(size=20),
          legend.key.size = unit(0.75,"line"),
          legend.text = element_text(size=16),
          axis.title.x=element_blank(),
          axis.text.x=element_text(color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
          plot.margin=unit(c(-0.7,0.05,3,0),"cm")))
  
  #  Draw figure with text annotations
  pg2b <- reactive(ggdraw(pg2()) + 
    annotate("text",x=0.5,y=0.065,label=paste0("NOAA Coral Reef Watch data, courtesy NOAA Pacific Islands Ocean Observing System (Updated: ",
                                               format(max(GOAdata$date),"%m-%d-%Y"),
                                               ")\n Data are modeled satellite products and periodic discrepancies or gaps may exist across sensors and products.\n                                    Contact: matt.callahan@noaa.gov, Alaska Fisheries Science Center "),
             hjust=0.5, size=7,family="sans",fontface=1,color=OceansBlue2,lineheight=0.85))
  pg3<-reactive(plot_grid(pg1(),pg2b(),ncol=1))
  
  output$GOAplot <- renderPlot({
    pg3()
  })
  
 
  ####-----------------------------------------------####
  #download buttons
  #image
  output$GOApng<-downloadHandler(
    filename = function() {
      paste("GOA-SST-", Sys.Date(), ".png", sep="")
    },
    contentType = "image/png",
    content= function(file){
      png(file, height=14.25, width=18, units="in", res=300)
      print(pg3())
      dev.off()
    })
  
  #Download button
  output$GOAData<-downloadHandler(
    filename = function() {
      paste("GOA-SST-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(GOAupdateddata, file)
    }) 
  
  #####-----------------------------------------------------------####
  # Aleuatians
  AIupdateddata <- base%>%
    filter(Ecosystem_sub %in% c("Western Aleutians", "Central Aleutians", "Eastern Aleutians"))
  
  
  AIdata <- 
    AIupdateddata %>% 
    rename_all(tolower) %>% 
    mutate(read_date=date,
           month=month(read_date),
           esr_region=factor(ecosystem_sub, levels=c("Western Aleutians","Central Aleutians","Eastern Aleutians")),
           day=day(read_date),
           year=year(read_date),
           newdate=as.Date(ifelse(month==12,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                  as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
           year2=ifelse(month==12,year+1,year)) %>% # To have our years go from Dec-Nov, force December to be part of the subsequent year.
    arrange(read_date) 
  ####-------------------------------------------------------####
  #plot
  #change legend position
  AIlegx <- 0.375
  #  Create plotting function that will allow selection of 3 ESR regions
  AIplotfun <- function(region1,region2,region3){
    mylines_base <- ggplot() +
      geom_line(data=AIdata %>% filter(year2<last.year & esr_region%in%(c(region1,region2,region3))), # Older years are grey lines.
                aes(newdate,meansst,group=factor(year2),col='mygrey'),size=0.3) +
      geom_line(data=AIdata %>% filter(year2==last.year & esr_region%in%(c(region1,region2,region3))), # The previous year
                aes(newdate,meansst,color='last.year.color'),size=0.75) +
      geom_line(data=AIdata %>% 
                  filter(year%in%mean.years & esr_region%in%(c(region1,region2,region3))) %>% # The mean from 1986-2015
                  group_by(esr_region,newdate) %>% 
                  summarise(meantemp=mean(meansst,na.rm=TRUE)),
                aes(newdate,meantemp,col='mean.color'),size=0.65,linetype="solid") +
      geom_line(data=AIdata %>% filter(year2==current.year & esr_region%in%(c(region1,region2,region3))), # This year
                aes(newdate,meansst,color='current.year.color'),size=0.75) +
      facet_wrap(~esr_region,ncol=3) + 
      scale_color_manual(name="",
                         breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                         values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                         labels=c(current.year,last.year,paste0('1985-',last.year-1),mean.lab)) +
      scale_linetype_manual(values=c("solid","solid","solid","dashed")) +
      scale_y_continuous(breaks=c(0,5,10))+
      ylab("Sea Surface Temperature (°C)") + 
      xlab("") +
      scale_x_date(date_breaks="1 month",
                   date_labels = "%b",
                   expand = c(0.025,0.025)) + 
      theme(legend.position=c(AIlegx,mylegy),
            legend.text = element_text(size=20,family="sans"),
            legend.background = element_blank(),
            legend.title = element_blank(),
            strip.text = element_text(size=24,color="white",family="sans",face="bold"),
            strip.background = element_rect(fill=OceansBlue2),
            axis.title.y = element_text(size=20,family="sans"),
            axis.text.y = element_text(size=16,family="sans"),
            panel.border=element_rect(colour="black",size=0.75),
            axis.text.x=element_blank(),
            legend.key.size = unit(0.35,"cm"),
            plot.margin=unit(c(-0.1,0.05,0,0),"cm")) 
    
    ggdraw(mylines_base)
  }
  
  pa1 <- reactive(AIplotfun("Eastern Aleutians","Central Aleutians","Western Aleutians") + 
    draw_image("Figures/fisheries_header_logo_jul2019.png",scale=0.2,x=mylogox,y=mylogoy,hjust=0.40))
  
  # Use heatwaveR package to detect marine heatwaves.
  AImhw <- (detect_event(ts2clm(AIupdateddata %>% 
                                  filter(Ecosystem_sub=="Eastern Aleutians") %>% 
                                  rename(t=date,temp=meansst) %>% 
                                  arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% 
    mutate(region=paste0(current.year," Eastern Aleutians Heatwaves")) %>% 
    bind_rows((detect_event(ts2clm(AIupdateddata %>%
                                     filter(Ecosystem_sub=="Central Aleutians") %>% 
                                     rename(t=date,temp=meansst) %>% 
                                     arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% 
                mutate(region=paste0(current.year," Central Aleutians Heatwaves"))) %>%
    bind_rows((detect_event(ts2clm(AIupdateddata %>%
                                     filter(Ecosystem_sub=="Western Aleutians") %>% 
                                     rename(t=date,temp=meansst) %>% 
                                     arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% 
                mutate(region=paste0(current.year," Western Aleutians Heatwaves")))
  
  
  #  Replace the current year with the previous year for our remaining days vector.
  AIdummydat <- data.frame(t=yearvec-current.year.length,newt=yearvec) %>%
    inner_join(AImhw %>% dplyr::select(region,thresh,seas,t)) %>% 
    dplyr::select(t=newt,region,thresh,seas) %>% 
    mutate(temp=NA)
  # Calculate threshold values for heatwave categories. This code directly from Schegel & Smit
  AIclim_cat <- AImhw %>%
    bind_rows(AIdummydat) %>% 
    group_by(region) %>% 
    dplyr::mutate(diff = thresh - seas,
                  thresh_2x = thresh + diff,
                  thresh_3x = thresh_2x + diff,
                  thresh_4x = thresh_3x + diff,
                  year=year(t),
                  region2=factor(region, levels=c(paste0(current.year," Western Aleutians Heatwaves"),
                                                  paste0(current.year," Central Aleutians Heatwaves"),
                                                  paste0(current.year," Eastern Aleutians Heatwaves")))) %>% 
    arrange(t)
  
  #  Create annotation text for plot
  AImhw_lab <- data.frame(region2=factor(c(paste0(current.year," Western Aleutians Heatwaves"),paste0(current.year," Central Aleutians Heatwaves"),paste0(current.year," Eastern Aleutians Heatwaves"))),
                          t=c(as_date(paste0(last.year,"-12-05")),as_date(paste0(last.year,"-12-05")),as_date(paste0(last.year,"-12-05"))),
                          temp=rev(c((1*max(AIclim_cat$temp,na.rm=TRUE)),(0.9*max(AIclim_cat$temp,na.rm=TRUE)),(0.8*max(AIclim_cat$temp,na.rm=TRUE)))),
                          mylab=rev(c("Heatwave intensity increases\n(successive dotted lines)\nas waters warm.",
                                      "Heatwaves occur when daily\nSST exceeds the 90th\npercentile of normal\n(lowest dotted line) for\n5 consecutive days.","")))
  pa2 <- reactive(ggplot(data = AIclim_cat %>% filter(t>=as.Date(paste0(last.year,"-12-01"))), aes(x = t, y = temp)) +
    geom_line(aes(y = temp, col = "Temperature"), size = 0.85) +
    geom_flame(aes(y2 = thresh, fill = Moderate)) +
    geom_flame(aes(y2 = thresh_2x, fill = Strong)) +
    geom_flame(aes(y2 = thresh_3x, fill = Severe)) +
    geom_flame(aes(y2 = thresh_4x, fill = Extreme)) +
    geom_line(aes(y = thresh_2x, col = "Strong (2x Threshold)"), size = 0.5, linetype = "dotted") +
    geom_line(aes(y = thresh_3x, col = "Severe (3x Threshold)"), size = 0.5, linetype = "dotted") +
    geom_line(aes(y = thresh_4x, col = "Extreme (4x Threshold)"), size = 0.5, linetype = "dotted") +
    geom_line(aes(y = seas, col = "Baseline"), size = 0.65,linetype="solid") +
    geom_line(aes(y = thresh, col = "Moderate (1x Threshold)"), size = 0.5,linetype= "dotted") +
    geom_text(data=AImhw_lab,aes(x=t,y=temp,label=mylab),hjust=0,size=8,family="sans",lineheight=1) +
    scale_colour_manual(name = NULL, values = lineColCat,
                        breaks = c("Temperature", "Baseline", "Moderate (1x Threshold)"),guide=FALSE) +
    scale_fill_manual(name = "Heatwave\nIntensity", values = c(Extreme,Severe,Strong,Moderate),labels=c("Extreme","Severe","Strong","Moderate")#, guide = FALSE
    ) +
    scale_x_date(limits=c(as_date(paste0(last.year,"-12-01")),as_date(paste0(current.year,"-11-30"))),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
    labs(y = "Sea Surface Temperature (°C)", x = NULL) + 
    facet_wrap(~region2,ncol=3) +
    mytheme + 
    theme(strip.text=element_text(size=20),
          legend.position=c(0.75,0.5),
          legend.title = element_text(size=20),
          legend.key.size = unit(0.75,"line"),
          legend.text = element_text(size=16),
          axis.title.x=element_blank(),
          axis.text.x=element_text(color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
          plot.margin=unit(c(-0.7,0.05,3,0),"cm")))
  pa2b <- reactive(ggdraw(pa2()) + 
    annotate("text",x=0.5,y=0.065,label=paste0("NOAA Coral Reef Watch data, courtesy NOAA Pacific Islands Ocean Observing System (Updated: ",
                                               format(max(AIdata$date),"%m-%d-%Y"),
                                               ")\n Data are modeled satellite products and periodic discrepancies or gaps may exist across sensors and products.\n                                    Contact: matt.callahan@noaa.gov, Alaska Fisheries Science Center "),
             
             hjust=0.5, size=7,family="sans",fontface=1,color=OceansBlue2,lineheight=0.85) )
  pa3<-reactive(plot_grid(pa1(),pa2b(),ncol=1))
  
  output$AIplot <- renderPlot({
    pa3()   
  })

  ####---------------------------------------------------------####
  #Download buttons
  #image
  output$AIpng<-downloadHandler(
    filename = function() {
      paste("AI-SST-", Sys.Date(), ".png", sep="")
    },
    contentType = "image/png",
    content= function(file){
      png(file, height=14.25, width=18, units="in", res=300)
      print(pa3())
      dev.off()
    })
  #csv
  output$AIData<-downloadHandler(
    filename = function() {
      paste("AI-SST-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(AIupdateddata, file)
    })    
  
}

shinyApp(ui = ui, server = server)
