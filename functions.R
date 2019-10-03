
################ Weather data download hourly ##########################
#thank you realmiketalbot 

library(jsonlite)
library(RCurl)
library(lubridate)
library(stringr)
library(magrittr)

###USER INPUTS###
iem.wd <-  getwd() #download location
date1 <- ISOdate(2005,1,1) #start date in year, month, day format
date2 <- ISOdate(year(today()),month(today()),day(today())) #end date in year, month, day format
user.network <- c("ASOS")
user.state <- c("MN") #state
user.faaid <- c("MSP") #site FAA identifier - leave empty and a list will print for your reference
#################

asoshourly<-function(iem.wd,date1,date2,user.network,user.state,user.faaid){

#create subdirectories
download.wd <- str_c(iem.wd, user.network, user.faaid, sep="/")
if(user.faaid != "") {dir.create(download.wd, recursive=T)}
setwd(download.wd)

service <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?"
service <- str_c(service, "data=all&tz=Etc/UTC&format=comma&latlon=yes&", sep="")
service <- str_c(service, "year1=", year(date1), "&month1=", month(date1), "&day1=", mday(date1), "&", sep="")
service <- str_c(service, "year2=", year(date2), "&month2=", month(date2), "&day2=", mday(date2), "&", sep="")

states <- c("AK AL AR AZ CA CO CT DE FL GA ")
states <- str_c(states,"HI IA ID IL IN KS KY LA MA MD ")
states <- str_c(states,"ME MI MN MO MS MT NC ND NE NH ")
states <- str_c(states,"NJ NM NV NY OH OK OR PA RI SC ") 
states <- str_c(states,"SD TN TX UT VA VT WA WI WV WY")

states <- unlist(strsplit(states, " "))

networks <- "AWOS"
for (i in 1:length(states)) {
  networks[i+1] <- str_c(states[i], "_ASOS", sep="")
}

if (user.network == "ASOS"){
  networks <- networks[which(networks %in% str_c(user.state, "_", user.network))]
} else {
  networks <- subset(networks %in% str_c(user.network))
}

for (network in networks){
  #get metadata
  uri <- str_c("https://mesonet.agron.iastate.edu/geojson/network/", network, ".geojson", sep="")
  
  data <- url(uri)
  jdict <- fromJSON(data)
  
  for (i in 1:nrow(jdict$features)){
    site <- jdict$features[i,]
    faaid <- site$properties$sid
    if (faaid == user.faaid) {
      sitename <- site$properties$sname
      uri <- str_c(service, "station=", faaid)
      print(str_c("Network:", network, "Downloading:", sitename, faaid, sep=" "))
      data <- url(uri)
      #print(data) #uncomment to print metadata
      datestring1 <- format(date1, "%Y%m%d")
      datestring2 <- format(date2, "%Y%m%d")
      outfn <- str_c(network, "_", faaid, "_", datestring1, "_to_", datestring2, sep="")
      download.file(uri, str_c(outfn, ".txt"), "auto")
    } 
    if (user.faaid == "" & i == 1) {
      print(data.frame(jdict$features$properties[c("sname", "sid")]))
    }
  }
}
}



#There must be a simpler way to just get the data I need. The URL is a nice API, so let's just do it. 

tz_data<-"America%2FChicago" #TZ

getMSPw<-function(station,year1,month1,day1,year2,month2,day2,tz_data){
      url<-paste0("http://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=",station,"&data=&year1=",year1,"&month1=",month1,"&day1=",day1,"&year2=",year2,"&month2=",month2,"&day2=",day2,"&tz=",tz_data,"&format=onlycomma&latlon=no&missing=empty&trace=T&direct=no&report_t")
      tmp<-tempfile()
      download.file(url,dest=tmp)
      as_tibble(read.table(tmp,sep=",",header=T))
      
}
    
td<-today()

getMSPw(station="MSP",2013,01,01,year2=year(td),month2=month(td),day2=day(td),tz_data = tz_data)
      
url2<-"http://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=MSP&data=all&year1=2013&month1=9&day1=1&year2=2019&month2=10&day2=3&tz=America%2FChicago&format=onlycomma&latlon=no&missing=empty&trace=T&direct=no&report_t"
tmp<-tempfile()
download.file(url2,dest=tmp)
test<-read.table(tmp,sep=",",header=T)
test %<>% as_tibble()
