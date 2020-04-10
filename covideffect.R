
library(tidyverse)
library(imputeTS)
library(ggthemes)
library(rnoaa)
library(readxl)
library(magrittr)
library(plotly)
library(nnet)
library(lubridate)


cv<-read_csv("./covid/loop2020.csv")


cv %<>% mutate_if(is.numeric, ~ na_interpolation(.x))


cv2<-cv %>% mutate(Timestamp=mdy_hm(Timestamp),
                   Month=month(Timestamp),
                   Year=year(Timestamp),
                   Hour=hour(Timestamp),
                   blckdkW=.25*blckkW,
                   Date=as.Date(Timestamp),
                   Week=week(Timestamp),
                   #Days=format(Date,format = "%d")
                   Days=yday(Timestamp) + Hour/24
                  ) %>%
            group_by(Month,Year,Hour,Date,Days,Week) %>%
            summarize(blckdkW=sum(blckdkW)) %>%
            ungroup() %>%
            filter(Week<16 & Week>8,is.na(blckdkW)==F,Date < Sys.time()) %>%
            mutate(md=paste0(Month,"-",Days,"-",Hour),
                   Year=as.factor(Year),
                   Timestamp=paste0(Date," ",Hour,":","00") %>% ymd_hm()) 
    



gg1<-cv2 %>% ggplot() + 
             geom_line(aes(x=Days,y=blckdkW,color=Year,text=Timestamp,alpha=Year)) + 
             scale_alpha_manual(values=c(0.5,0.5,0.5,1.5)) +
             scale_color_manual(values=c("blue","orange","lightskyblue","black")) +
             xlab("Day of year (weeks 9-15)") + 
             scale_y_continuous("Block Demand Real Power kW",breaks=seq(0,8000,500))
            



gg1


htmlwidgets::saveWidget(as_widget(gg1), selfcontained = T, "powerDemand.html")
