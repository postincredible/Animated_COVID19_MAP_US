

###############################################
##                                           ##
##      R code for animated mapping the      ##
##      confirmed case in the US             ##
##                                           ##
##              Yihe Huang                   ##
##             version: 0.2                  ##
##             date: 06-09-2020              ## 
##                                           ##
###############################################

library(usmap)
library(ggplot2)
library(magick)
library(tidyverse)

# Read files from source 02-02-2020 to 06-08-2020
start_date <- '2020-07-23'
end_date <- '2020-07-25'

d0 <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/', format(seq(as.Date(start_date), as.Date(end_date), by = '1 days'), '%m-%d-%Y'),  '.csv')

world_covid <- read_csv(url(d0[2])) %>% filter(Country_Region == 'US') 

world_covid %>% count(Province_State)
world_covid <- read_csv(url(d0[2]))

### Uncomment following code to check the variable used for selecting country and states
#for (i in 1:length(d0)) {
#  date = d0[i]
#  world_covid <- read_csv(url(date))
#  l0 <- tail(unlist(strsplit(date,'/')),n=1)
#  repdate <- unlist(strsplit(l0,'.csv'))
#  country_filter <- ifelse(sum(names(world_covid) %>% str_detect('Country/Region')) == 1,'Country/Region', 
#                           ifelse(sum(names(world_covid) %>% str_detect('Country_Region')) == 1, 'Country_Region')
#  )
#  
#  state_filter <- ifelse(sum(names(world_covid) %>% str_detect('Province/State')) == 1,'Province/State', 
#                         ifelse(sum(names(world_covid) %>% str_detect('Province_State')) == 1, 'Province_State')
#  )
#  cat('#',i,': ',repdate,' Cty var: ', country_filter, 'Ste var: ', state_filter, '\n')
#}
###

roundUp <- function(x,to=10)
{
  to*(x%/%to + as.logical(x%%to))
}

# Set single image size and resolution 
#img0 <- image_graph(600, 450, res = 96)
img0 <- image_graph(1366, 768, res = 192)

# Get animation map
out0 <- lapply(d0, function(date){
  
  # load covid case all over the world
  world_covid <- read_csv(url(date))
  
  # get the date info
  l0 <- tail(unlist(strsplit(date,'/')),n=1)
  repdate <- unlist(strsplit(l0,'.csv'))
  
  ## variable name for getting country and state
  country_filter <- ifelse(sum(names(world_covid) %>% str_detect('Country/Region')) == 1,'Country/Region', 
                           ifelse(sum(names(world_covid) %>% str_detect('Country_Region')) == 1, 'Country_Region')
  )
  
  state_filter <- ifelse(sum(names(world_covid) %>% str_detect('Province/State')) == 1,'Province/State', 
                         ifelse(sum(names(world_covid) %>% str_detect('Province_State')) == 1, 'Province_State')
  )
  

  
  
  ## plot data on date <= '03-09-2020'
  if (repdate <= '03-09-2020') {
    # get the covid case in usa on the date
    usa_covid <- world_covid %>%
      #filter(`Country/Region`=='US') %>% 
      filter(UQ(rlang::sym(country_filter)) == 'US') %>% 
      separate(UQ(rlang::sym(state_filter)), c('City','abbr'), sep=', ') %>% 
      separate(abbr,c('state','ship'), sep=' ') %>% 
      filter(is.na(ship) & !is.na(state)) %>%
      select(-c(ship)) %>%
      drop_na()  
    
    usa_covid_sum <- usa_covid %>%
      group_by(state) %>% 
      summarise(sum_conf=sum(Confirmed))
    
    usa_covid_sum <- left_join(usa_covid_sum, statepop, by = c('state' = 'abbr'), copy = FALSE) %>% 
      add_column(date=repdate)
    
    max_cfm <- max(usa_covid_sum$sum_conf)
    cfm_digits <- floor(log10(max(usa_covid_sum$sum_conf))) 
    maxcom <- roundUp(x= max_cfm, to=10^cfm_digits)
    
    
    
    p0=plot_usmap(data = usa_covid_sum, values = "sum_conf", color = "red", labels = F) + 
      scale_fill_continuous(
        low = "cornsilk", high = "red1", name = "Confirmed (nCov19)", na.value = "white", limits= c(0,maxcom)
      ) + 
      theme(legend.position = c(0.9,.11), legend.direction = 'vertical') + 
      labs(caption = usa_covid_sum$date)  + 
      theme(plot.caption = element_text(hjust=0.6,size=20)) + 
      theme(legend.title =  element_text(size=10)) + 
      theme(legend.text =  element_text(size=10))
    
    
    print(p0)
    
  } # end plot data on date <= '03-09-2020'
  
  
  
  
  ## plot data on date >= '03-10-2020'
  if (repdate >= '03-10-2020') {
    # get the covid case in usa on the date
    usa_covid <- world_covid %>%
      filter(UQ(rlang::sym(country_filter)) == 'US') %>% 
      filter( UQ(rlang::sym(state_filter)) %in% statepop$full ) %>%
      #separate(UQ(rlang::sym(state_filter)), c('City','abbr'), sep=', ') %>% 
      #separate(abbr,c('state','ship'), sep=' ') %>% 
      #filter(is.na(ship) & !is.na(state)) %>%
      #select(-c(ship)) %>%
      drop_na()  
    
    usa_covid_sum <- usa_covid %>%
      group_by(UQ(rlang::sym(state_filter))) %>% 
      summarise(sum_conf=sum(Confirmed))
    
    

    usa_covid_sum <- left_join(usa_covid_sum, statepop, by = structure(names = state_filter, .Data = 'full'), copy = FALSE) %>% 
      add_column(date=repdate)
    
    max_cfm <- max(usa_covid_sum$sum_conf)
    cfm_digits <- floor(log10(max(usa_covid_sum$sum_conf))) 
    maxcom <- roundUp(x= max_cfm, to=10^cfm_digits)
    
    
    p0=plot_usmap(data = usa_covid_sum, values = "sum_conf", color = "red", labels = F) + 
      scale_fill_continuous(
        low = "cornsilk", high = "red1", name = "Confirmed\n (nCov19)", na.value = "white", limits= c(0,maxcom)
      ) + 
      theme(legend.position = c(0.9,.11), legend.direction = 'vertical') + 
      labs(caption = usa_covid_sum$date)  + 
      theme(plot.caption = element_text(hjust=0.6,size=20)) + 
      theme(legend.title =  element_text(size=10)) + 
      theme(legend.text =  element_text(size=10))
    
    
    print(p0)
    
  } # end plot data on date >= '03-10-2020'
  
} )
dev.off()
animation <- image_animate(img0, fps = 2)
#print(animation)

## check the current working directory for saving the animated .gif
getwd()

image_write(animation, "covid-19us.gif")


