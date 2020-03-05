

library(usmap)
library(ggplot2)
library(readr)
library(tidyr)
library(tibble)
library(dplyr)
library(magick)

# Read files from source 02-02-2020 to 03-03-2020
d0 <- c(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/02-0", 2:9,'-2020.csv'), 
        paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/02-", 10:29,'-2020.csv'), 
        paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-0", 1:3,'-2020.csv'))

# Set single image size and resolution 
#img0 <- image_graph(600, 450, res = 96)
img0 <- image_graph(1366, 768, res = 192)

# Get animation map
out0 <- lapply(d0, function(date){
  
  mydata<-read_csv(url(date))
  
  l0=tail(unlist(strsplit(date,'/')),n=1)
  repdate=unlist(strsplit(l0,'.csv'))
  
  usa=mydata[which(mydata$`Country/Region`=='US'),]
  
  
  usa=usa %>% separate(`Province/State`,c('City','abbr'), sep=', ')
  usa= usa %>% drop_na()
  usb=usa %>% group_by(abbr) %>% summarise(sum_conf=sum(Confirmed))
  usb=left_join(usb, statepop, by = c('abbr'), copy = FALSE)
  usb= usb %>% drop_na()
  usb= usb %>% add_column(date=repdate)
  
  p0=plot_usmap(data = usb, values = "sum_conf", color = "red", labels = F) + 
    scale_fill_continuous(
      low = "cornsilk", high = "red1", name = "Confirmed (nCov19)", na.value = "white", limits= c(0,30)
    ) + theme(legend.position = c(0.9,.11), legend.direction = 'vertical') + labs(caption = usb$date)  + 
    theme(plot.caption = element_text(hjust=0.6,size=20)) + theme(legend.title =  element_text(size=10)) + 
    theme(legend.text =  element_text(size=10))
  
  
  print(p0)
})
dev.off()
animation <- image_animate(img0, fps = 2)
#print(animation)

getwd()

image_write(animation, "covid-19us.gif")
