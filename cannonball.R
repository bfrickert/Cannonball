library(dplyr)
library(plotly)
cannonball <- read.table('data/bolian.report2.tsv',sep='\t',header=T, fill=T, stringsAsFactors = F)

cannonball <- filter(cannonball, !grepl('US-[A-Z]+',Location.Name)==F)
cannonball$State <- gsub('US-', '', unlist(lapply(strsplit(cannonball$Location.Name, split=', '), 
              function(x) x[grepl('US-[A-Z]+',x)])))
cannonball$id <- 1:nrow(cannonball)


plot_ly(data=cannonball, x=Speed, type='histogram')
plot_ly(data=cannonball, y=Speed, type='box')

speed.by.state <- as.data.frame(arrange(summarise(group_by(cannonball, State), median.speed=median(Speed), 
                                    mean.speed=mean(Speed), min.speed=min(Speed), max.speed=max(Speed),
                                    arrived.in.state=min(Reported.At..US.Eastern.),
                                    departed.state=max(Reported.At..US.Eastern.)), 
                          desc(median.speed)))
speed.by.state$duration.sec <- as.vector(difftime(as.POSIXct(speed.by.state$departed.state, format="%Y-%m-%dT%H:%M:%S"), 
         as.POSIXct(speed.by.state$arrived.in.state, format="%Y-%m-%dT%H:%M:%S"), units='sec'))
speed.by.state$duration.min <- speed.by.state$duration.sec/60
speed.by.state$duration.hour <- speed.by.state$duration.min/60
speed.by.state
arrange(speed.by.state, arrived.in.state)
summary(cannonball$Speed)

geo <- g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

plot_ly(cannonball, lon = Longitude, lat = Latitude, text = paste(Location.Name, '<br>Speed: ', Speed), 
        type = 'scattergeo',
        locationmode = 'USA-states', marker = list(size = 2, color = 'red'), mode='markers',
        inherit = FALSE) %>%
  add_trace(lon = lon, lat = lat, data=data.frame(lat = 32.2217, lon ="-110.9264"), 
            marker = list(size=8, color='black', symbol='cross'),
            type = 'scattergeo', locationmode = 'USA-states', 
            text='Tucson, Arizona') %>%
  add_trace(lon = lon, lat = lat, data=data.frame(lat=33.8564, lon ="-118.3769"), 
            marker = list(size=8, color='red', symbol='circle'),
            type = 'scattergeo', locationmode = 'USA-states', text='Redondo Beach, California') %>%
  add_trace(lon = lon, lat = lat, data=data.frame(lat=40.7127, lon="-74.0059"), 
            marker = list(size=8, color='red', symbol='circle'),
            type = 'scattergeo', locationmode = 'USA-states', text='New York, New York') %>%
  layout(title = 'Ed Bolian\'s course',
         geo = geo, showlegend = FALSE)

cars <- read.table('data/cannonball.tsv', sep='\t', fill= T, stringsAsFactors = F, header = T)
t <- list(
  family = "sans serif",
  size = 18,
  color = toRGB("grey50")
)
plot_ly(cars, x = bph, y = torque.lbs.ft, text = car, mode = "markers+text",
        textfont = t, textposition = "top middle")

plot_ly(cars, x = top.speed, y = mpg, text = car, mode = "markers+text",
        textfont = t, textposition = "top middle")


plot_ly(cannonball, x = factor(State), y=Speed, type='box')
cannonball$time.gap <- 0
cannonball$motion.gap <- 0
cannonball$idle.gap <- 0

cannonball[2:length(cannonball$Reported.At..US.Eastern.),]$time.gap <-
  as.vector(difftime(as.POSIXct(cannonball[2:length(cannonball$Reported.At..US.Eastern.),]$Reported.At..US.Eastern., format="%Y-%m-%dT%H:%M:%S"), 
                     as.POSIXct(cannonball$Reported.At..US.Eastern.[1:length(cannonball$Reported.At..US.Eastern.)-1], format="%Y-%m-%dT%H:%M:%S"), 
                     units='sec'))

cannonball[2:length(cannonball$Reported.At..US.Eastern.),]$motion.gap <-
  as.vector(cannonball[2:length(cannonball$Reported.At..US.Eastern.),]$X4..In.Motion.Time..s. - 
              cannonball$X4..In.Motion.Time..s.[1:nrow(cannonball)-1])

cannonball[2:nrow(cannonball),]$idle.gap <-
  as.vector(cannonball[2:nrow(cannonball),]$X2..Idle.Time..s. - 
              cannonball$X2..Idle.Time..s.[1:nrow(cannonball)-1])

plot_ly(data.frame(arrange(summarise(group_by(filter(cannonball, idle.gap >0), State), min.time=min(Reported.At..US.Eastern.), idle.gap.sum=sum(idle.gap)), min.time)),
        x=factor(State), y=idle.gap.sum, type='bar')
