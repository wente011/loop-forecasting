#THis would be a script to quickly look 

library(tidyverse)
library(imputeTS)
library(rnoaa)
library(readxl)
library(magrittr)
library(nnet)
library(lubridate)


############################################# Grab and process demand and solar data #####################################

data<-read_csv("loop-solar.csv")

data %<>% mutate_if(is.numeric,function(x) na_interpolation(x)) %>% mutate(solar=(`MSB.PV_Inverter_1#Real Power#kW` + 
                                                                                    `MSB.PV_Inverter_2#Real Power#kW` +
                                                                                  `MSB.PV_Inverter_3#Real Power#kW`))  
                                                                    
data$dd<-data$`Virtual.zzXcel_Total#Block Demand Real Power#kW`


data %<>% select(dd,solar,Timestamp)

data$Timestamp %<>% mdy_hm()

pks<- data %>% mutate(MY=paste0(month(Timestamp),"-",year(Timestamp))) %>% 
                mutate(id=seq(1,length(Timestamp))) %>%
                group_by(MY) %>% 
                summarize(pks=max(dd,na.rm=T),
                          ranks=which.is.max(dd),
                          time=Timestamp[ranks],
                          solar=solar[ranks]
                          )

pks2<-pks %>% select(time) %>% rename(Timestamp="time") %>% mutate(pktime=1)


gg1<-pks %>% ggplot(aes(x=time,y=pks/1000,color="MW Load")) + geom_line() + geom_line(aes(x=time,y=solar,color="Solar kW"))
gg1

pks$Date<-date(pks$time)
data$Date<-date(data$Timestamp)

data$pkday<-ifelse(data$Date %in% pks$Date,1,0)
data2<-left_join(data,pks2)
data2$pktime[is.na(data2$pktime)]<-0


#Now just need to add a line for the peak demand.



gg2<-data2 %>% filter(pkday==1) %>% mutate(MY=paste0(month(Timestamp),"-",year(Timestamp))) %>%
                              mutate(Hour=hour(Timestamp) + minute(Timestamp)/60,pktime2=Hour*pktime %>% 
                                                                                  ifelse(.==0,NA,.)) %>%
                              
                              ggplot(aes(x=Hour,y=dd/100,color="kW Demand/100")) + geom_line() +
                              geom_line(aes(x=Hour,y=solar,color="Solar kW")) + 
                              geom_vline(aes(xintercept = pktime2,color="Peak Demand Time")) + 
                              facet_wrap(~Date,drop=T) + 
                              scale_x_continuous("Hour in Day",breaks=seq(0,23,2)) + 
                              theme(axis.text.x = element_text(angle = 45)) 
 

gg2                           





############################################# Pull in Weather Data and Irradiation Data ###################################################################

#St. Paul Irradiance data. First run weatherdta.R.


irr<-read_excel("St Paul Solar_1963_2019.xlsx",sheet="data") %>% 
                gather("year","irr",-Month) %>% 
                mutate(Date=paste0(year,"-",Month,"-","01") %>% ymd()
                       )

ghcnd_clear_cache(force=TRUE)

wd<-meteo_tidy_ghcnd(stationid = "USW00014922",date_min = "2017-01-01",date_max = "2020-03-02") %>% 
        mutate(Date=paste0(year(date),"-",month(date),"-","01") %>% ymd(), 
               tmean=((tmax+tmin)/2)/10*(9/5)+32,       #convert to F
               snow=snow*0.0393701                      #convert to inches 
               ) %>% 
        group_by(Date) %>% 
        summarize(snow=sum(snow,na.rm=T),
                  tmean=mean(tmean)
        )


wd2<-left_join(irr,wd) #a neat join.

#Now to join the solar irradiance data

monthly<-data2 %>% mutate(
                        solar=solar*.25,
                        dd=dd*.25,
                        Date=paste0(year(Timestamp),"-",month(Timestamp),"-","01") %>% ymd()
                          ) %>%
                group_by(Date) %>%
                summarize(peak=max(dd/.25),
                          dd.mwh=sum(dd)/1000,
                          solar.kwh=sum(solar),
                          solar.pk=max(solar/.25)
                          ) %>%
                left_join(wd2) %>%
                ungroup()


ir.plot<-irr %>% group_by(year) %>%
                  summarize(irr=mean(irr)) %>% mutate(year=as.numeric(year)) %>%
                  ggplot(aes(x=year,y=irr)) + geom_line()
  
ir.plot



gg4<-monthly %>% ggplot(aes(x=irr,y=solar.kwh,size=snow,color=tmean)) + geom_point() + geom_smooth() + 
                  scale_y_continuous("MSB Solar Production (kWh)",breaks=seq(0,25000,1000),label=scales::comma) + 
                  scale_x_continuous("Average Solar Radiation, Langleys per day ")
               
gg4









