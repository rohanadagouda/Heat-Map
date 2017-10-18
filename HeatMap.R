library(leaflet)
library(dplyr)
library(htmlwidgets)
library(png)
library(webshot)
library(magick)
library(animation)
library(htmltools)
library(viridis)

#Loading the Data
Location <- read.csv("Safecity_Crimes_Delhi.csv",header = TRUE,fill = TRUE)

#Converting to Numeric
Location$LATITUDE <- as.numeric(Location$LATITUDE)

#Converting to Numeric
Location$LONGITUDE <- as.numeric(Location$LONGITUDE)


#Filtering the Data
Year_Count <- Location %>% filter(VERBAL.ABUSE %in% c(1))%>% select(YEAR)

#Getting the Count of number of Years
Year_Count <- as.data.frame(table(Year_Count$YEAR))
colnames(Year_Count) <- c("Year","Count")

#Generating series of PNG file of based on the years
for (i in 1:NROW(Year_Count)) {
  Location_Year <- Location %>% filter(VERBAL.ABUSE %in% c(1))%>% select(LONGITUDE,LATITUDE,YEAR)
  
  Location_Year <- Location_Year %>% filter(YEAR==Year_Count$Year[i])
  
  VB <-leaflet(data =Location_Year)%>%
    addTiles() %>% 
    setView(lng = 77.209021,lat =28.613939,zoom = 11)%>%
    addProviderTiles('OpenStreetMap') %>% 
    addCircles(~LONGITUDE,~LATITUDE,color = "red",opacity = 0.8)%>%
    addLegend("bottomright",colors = "red",
              labels = Year_Count$Year[i],title = "Year")
  
  saveWidget(VB,file = 'temp.html', selfcontained = FALSE) ## save the html
  webshot('temp.html', file=sprintf('Rplot%02d.png',i),
          cliprect = 'viewport') ##Capture the image
}

#Reading PNG files to a common object to genarate a GIF
png.files <- sprintf("Rplot%02d.png", 1:NROW(Year_Count))

#Need to create  magick image object for animation and GIF
animation <- image_read(png.files)
animation <- image_animate(animation,fps = 1)

#Converting the Images to GIF
image_write(animation, "VERBAL.ABUSE.gif")

#Removing the PNG files
file.remove(list.files(pattern=".png"))