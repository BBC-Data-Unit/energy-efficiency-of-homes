rm(list=ls())
setwd('D:/R/R data/R_training')
.libPaths('D:/R/Libraries')
library(maps)
library(mapdata)
library(ggplot2)
library(rgeos)
library(broom)
library(plyr)
library(readxl)
library(janitor)
library(tmap)
library(tidyverse)
library(janitor)
library(shiny)
library(shinyjs)
library(ggmap)
library(maptools)
library(rgdal)
library(readxl)

#Load the shapefile
scot_shapes <- readOGR(dsn="D:/R/R data/R_training", layer="SG_LAULevel1_2019")

names(scot_shapes)
### read in my data

scot_ee_map <- read_xlsx("scot_ee_map.xlsx")
options(scipen = 999)

#appears to work
scot_la_join <- merge(scot_shapes, scot_ee_map, by="name") 


#testing it returns a shapefile
class(scot_la_join)

names(scot_ee_map)
#testing la borders present
scot_la_layer <- tm_shape(scot_la_join)+
  tm_fill()+
  tm_borders()
scot_la_layer

#adding incident values to plot with different break styles
scot_ee_co2_map <- scot_la_layer +
  tm_shape(scot_la_join)+
  tm_polygons(col='avg_co2', title= "Avg CO2 emissions", alpha= 0.7, style='pretty', showNA=FALSE)
scot_ee_co2_map

scot_ee_pot_c02 <- scot_la_layer+
  tm_shape(scot_la_join)+
  tm_polygons(col='avg_co2_pot', title= "Avg C02 after upgrades ", alpha= 0.7, style='pretty',showNA=FALSE)
scot_ee_pot_c02

tmap_arrange(scot_ee_co2_map, scot_ee_pot_c02)

scot_ee_cost <- scot_la_layer+
  tm_shape(scot_la_join)+
  tm_fill('avg_total_cost', title= "Cost millions (£)", style='equal', showNA=FALSE,
          breaks = c(breaks=c(0,50,100,150,200,250,300, 350, Inf))) +
  tm_borders() +
  tm_layout("Avg total cost",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
scot_ee_cost


scot_ee_save <- scot_la_layer+
  tm_shape(scot_la_join)+
  tm_fill('avg_total_savings', title= "Savings (£)", style='equal', showNA=FALSE) +
  #breaks = c(breaks=c(0,200,400,600,800,1000, Inf))) +
  tm_borders() +
  tm_layout("Avg savings",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
scot_ee_save

tmap_arrange(scot_ee_cost, scot_ee_save)

scot_ee_roof <- scot_la_layer +
  tm_shape(scot_la_join)+
  tm_polygons(col='no_no_roof_insulation', title= "No roof Insulation", alpha= 0.7, showNA=FALSE)
scot_ee_roof + tm_style("cobalt")


scot_ee_wall <- scot_la_layer +
  tm_shape(scot_la_join)+
  tm_polygons(col='no_no_wall_insulation', title= "No wall Insulation", alpha= 0.62, showNA=FALSE)
scot_ee_wall + tm_style("cobalt")

wall_final <- scot_ee_wall + tm_style("cobalt")
roof_final <- scot_ee_roof + tm_style("cobalt")

tmap_arrange(roof_final, wall_final)

ee_save + tm_style("bw")
ee_save + tm_style("classic")
ee_save + tm_style("cobalt")
ee_save + tm_style("col_blind")


#### Scotland

#Load the shapefile
shapefile <- readOGR(dsn="D:/R/R data/R_training", layer="Local_Administrative_Units_Level_1_December_2015_Full_Clipped_Boundaries_in_England_and_Wales")

names(shapefile)
### read in my data

ee_map <- read_xlsx("EE MAP.xlsx")
options(scipen = 999)

#appears to work
la_join <- merge(shapefile, ee_map, by="lau115nm") 


#testing it returns a shapefile
class(la_join)

#testing la borders present
la_layer <- tm_shape(la_join)+
  tm_fill()+
  tm_borders()
la_layer



names(ee_map)
#adding incident values to plot with different break styles
ee_co2_map <- la_layer +
  tm_shape(la_join)+
  tm_polygons(col='average_co2_emissions', title= "Avg CO2 emissions", alpha= 0.7, style='pretty')
ee_co2_map

ee_pot_c02 <- la_layer+
  tm_shape(la_join)+
  tm_polygons(col='average_pot_cO2_after_improve', title= "Avg C02 after upgrades ", alpha= 0.7, style='pretty')
ee_pot_c02

ee_cost <- la_layer+
  tm_shape(la_join)+
  tm_fill('average_total_improve_cost', title= "Cost millions (£)", style='equal', showNA=FALSE,
          breaks = c(breaks=c(0,200,400,600,800,1000, Inf))) +
  tm_borders() +
  tm_layout("Avg improvement cost",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
ee_cost


ee_save <- la_layer+
  tm_shape(la_join)+
  tm_fill('average_total_savings', title= "Savings (£)", style='equal', showNA=FALSE) +
  #breaks = c(breaks=c(0,200,400,600,800,1000, Inf))) +
  tm_borders() +
  tm_layout("Avg savings",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.alpha = 1)
ee_save

ee_roof <- la_layer +
  tm_shape(la_join)+
  tm_polygons(col='no_roof_insulation', title= "No roof Insulation", alpha= 0.7, showNA=FALSE)
ee_roof + tm_style("cobalt")


ee_wall <- la_layer +
  tm_shape(la_join)+
  tm_polygons(col='no_wall_insulation', title= "No wall Insulation", alpha= 0.7, showNA=FALSE)
ee_wall + tm_style("cobalt")
ee_wall

tmap_arrange(ee_roof + tm_style("cobalt"),ee_wall + tm_style("cobalt"))


