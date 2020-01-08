library(rnoaa)
library(forecast)
library(tidyverse)
library(imputeTS)
library(magrittr)
library(RColorBrewer)
library(lubridate)
wd<-"https://www.ncdc.noaa.gov/cdo-web/api/v2/{endpoint}"
options(noaakey = tok)

ghcnd_clear_cache()
data<-meteo_tidy_ghcnd("USW00014922", keep_flags = FALSE, var = "all",
                 date_min = NULL, date_max = "2019-12-17")

data$prcp[9:length(data$prcp)]<-na_interpolation(data$prcp[9:length(data$prcp)])



data$prcp<-data$prcp/10*0.0393701    #converting the prcp to inches.

#prcp<-msts(data$prcp,seasonal.periods=c(2,365),start="1938-04-01",end="2019-08-31")

#prcp %>% autoplot()

data %<>% mutate(Month=month(date)) %>% mutate(Year=year(date))

data$count<-ifelse(data$prcp>2.49,1,0)

liss<-tibble::lst(mean, median,max,min,sum)

dmon<-data %>% group_by(Year,Month) %>% summarize_at("prcp",liss) %>% mutate(Date=mdy(paste0(Month,"/","01","/",Year)))

dyear<-data %>% group_by(Year) %>% summarize_at("prcp",liss,na.rm=T) %>% mutate(pct_chng=(sum-24.5)/24.5) %>%
  mutate(Date=ymd(str_c(Year,"01-01",sep="-")))

dco<-data %>% group_by(Year) %>% summarize_at("count",sum,na.rm=T)

dyear %<>% left_join(dco)

dmon %>% ggplot(aes(x=Date,y=max)) + geom_point() + geom_smooth(method="lm",se=T)
dyear %>% ggplot(aes(x=Year,y=count)) + geom_point() + geom_smooth(method="lm",se=T) #+ scale_y_continuous(labels=scales::percent) 


########################## Water stripe ###########################

theme_strip <- theme_minimal()+
 
   theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 3),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold")
  )


#rev() function infront of brewer.pal can reverse the colors 

col_strip <- rev(brewer.pal(9, "Blues"))

brewer.pal.info
  
ggplot(dyear,
       aes(x = Date, y = 1, fill = sum))+
  geom_tile()+
  scale_x_date(date_breaks = "6 years",
               date_labels = "%Y",
               expand = c(0, 0),limits=c(as.Date("1939-01-01"),as.Date("2019-01-01")),)+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "Precipitation at MSP 1939-2018",
       caption = "Cumulative Precipitation (inches)" )+
  theme_strip  
  
  

