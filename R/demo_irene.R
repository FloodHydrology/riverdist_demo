#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Riverdist Demo
#Coder: Nate Jones
#Date: 5/8/2021
#Purpose: Help Irene with spatial analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup Workspace -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear Memory
remove(list=ls())

#Load libraries of interest
library(tidyverse) #general data wrangling
library(sf)        #vector data
library(riverdist) #rivdist
library(mapview)   #web maps!

# lat/long coordinate reference of input data (see spatialreference.org)
p <- '+proj=longlat +datum=WGS84 +no_defs'

#Read in sampling points
pnts <- read_csv("data//Sipsey_sites.csv") %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs= p )

#load streams file
streams<- st_read("data//Sipsey_line.shp", crs = p) 

#plot for funzies
streams %>% plot(., col='dark blue', lwd=2)
pnts %>% plot(., pch=19, col='dark red', cex=2, add=T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: rivdist estimate ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Setup workspace ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create a temp folder to write network folder too
temp_file<-tempdir()
dir.create(temp_file)

#Reproject vector files to planar coordinates (UTM 16N; https://spatialreference.org/ref/epsg/26916/proj4/)
p_new<-"+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
streams<-st_transform(streams, crs=p_new)
pnts<-st_transform(pnts, crs=p_new)

#Prep flownet~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Wrangle streams 
streams <- streams %>% 
  #remove z information (to mmake it a 'linear' feature)
  st_zm() %>% 
  #Add ID data
  mutate(
    uid = seq(1, nrow(.)),
    type = 'river'
  )

#export streams to temp file
st_write(streams, paste0(temp_file,"\\streams.shp"), delete_dsn = T)

#Create flownet
flow_net <- line2network(path=temp_file, layer="streams", tolerance = 1)

#save flow network 
save(flow_net, file = paste0(temp_file, "\\riv.rda"))

#Prep sampling points ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Wrangle sampling points
pnts<-pnts %>% 
  #Define coordinates
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2],
  ) %>% 
  #Add site neame
  mutate(site_name = 'Site Name') %>% 
  st_drop_geometry()

#Snap points to flow network
snap<-xy2segvert(x=pnts$x, y=pnts$y, rivers=flow_net)

#Estimate distances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define river distance between points!
output<-riverdistancemat(
  seg = snap$seg, 
  vert = snap$vert, 
  rivers = flow_net)
