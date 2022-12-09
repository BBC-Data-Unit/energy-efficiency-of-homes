theme_set(theme_light())
setwd('D:/R/R data/R_training')
.libPaths('D:/R/Libraries')
library(tidyverse)
library(readxl)
library(janitor)
library(data.table)


# Get a List of all files in directory named with a key word, say all `.csv` files
filenames <- list.files("D:/R/R data/R_training", pattern="*extract0517.csv", full.names=TRUE)

# read and row bind all data sets
scot_epc <- rbindlist(lapply(filenames,fread))

view(filenames)

scot_epc <- clean_names(scot_epc)
names(scot_epc)


#### new code
postcode <- read.csv("Postcode sheet 1.csv") %>% 
  select(2,19) %>% 
  distinct() %>% 
  rename(postcode=Postcode.Lookup)


scot_epc  %>% 
  rename(postcode=postcode_sector)

postcode_pivot <- postcode %>% 
  group_by(postcode) %>% 
  summarise(total=n()) %>%
  arrange(desc(total))

#filtering down to just single match postcodes
postcode_single_only <- postcode_pivot %>% 
  filter(total==1)

postcode <- left_join(postcode_single_only,postcode) %>% 
  select(-2)

lookup <- postcode

lookup$postcode <- gsub("[A-Z]$","",lookup$postcode)
lookup$postcode <- gsub("[A-Z]$","",lookup$postcode)

#!!!! THIS IS A DELIBERATE REPETITION!!!

lookup <- lookup %>% 
  distinct()

lookup_no_name <- lookup %>%
  select(1) %>% 
  group_by(postcode) %>% 
  summarise(total=n()) %>% 
  filter(total==1)

lookup <- left_join(lookup_no_name,lookup) %>% 
  select(-"total")

#scot_master <- left_join(scot_epc,test, by = "postcode")

scot_energy <- scot_epc %>% 
  rename(postcode=postcode_sector)

#creating column to turn into id
scot_energy <-  scot_energy %>% 
  mutate(id_blank=dwelling_type)

#populating id column with row numbers
scot_energy$id_blank <- seq.int(nrow(scot_energy))

scot_energy <- scot_energy[, c(50,1,2:49)]

#merging with postcode lookup
scot_master <- left_join(scot_energy,lookup, by = "postcode")
#scot_master <- scot_master[, c(1,51,2:50)]
#scot_master <- distinct(scot_master)

# creating filter for LA

east_lothian <- scot_master %>% 
  filter(LA_Name == "East Lothian") 

### what number of properties per grade

scot_east_lothian_summary <-  east_lothian %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
 spread(current_energy_efficiency_rating_band, number_of_obs)

scot_east_lothian_summary["la_name"] <- "East Lothian"
scot_east_lothian_summary <- scot_east_lothian_summary[colnames(scot_east_lothian_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- east_lothian %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_east_lothian_summary <- scot_east_lothian_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- east_lothian %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_east_lothian_summary <- scot_east_lothian_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- east_lothian %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))
 
scot_east_lothian_summary <- scot_east_lothian_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- east_lothian %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_east_lothian_summary <- scot_east_lothian_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- east_lothian %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_lothian)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- east_lothian %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_lothian)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- east_lothian %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_lothian)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- east_lothian %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- east_lothian %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- east_lothian %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- east_lothian %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### Aberdeen City

# creating filter for LA

aberdeen_city <- scot_master %>% 
  filter(LA_Name == "Aberdeen City") 

### what number of properties per grade

scot_aberdeen_city_summary <-  aberdeen_city %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_aberdeen_city_summary["la_name"] <- "Aberdeen City"
scot_aberdeen_city_summary <- scot_aberdeen_city_summary[colnames(scot_aberdeen_city_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- aberdeen_city %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_aberdeen_city_summary <- scot_aberdeen_city_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- aberdeen_city %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_aberdeen_city_summary <- scot_aberdeen_city_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- aberdeen_city %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_aberdeen_city_summary <- scot_aberdeen_city_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- aberdeen_city %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_aberdeen_city_summary <- scot_aberdeen_city_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- aberdeen_city %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(aberdeen_city)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_aberdeen_city_summary <- scot_aberdeen_city_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- aberdeen_city %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(aberdeen_city)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_aberdeen_city_summary <- scot_aberdeen_city_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- aberdeen_city %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(aberdeen_city)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_aberdeen_city_summary <- scot_aberdeen_city_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- aberdeen_city %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_aberdeen_city_summary <- scot_aberdeen_city_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- aberdeen_city %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_aberdeen_city_summary <- scot_aberdeen_city_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- aberdeen_city %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_aberdeen_city_summary <- scot_aberdeen_city_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- aberdeen_city %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_aberdeen_city_summary <- scot_aberdeen_city_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)

### Aberdeenshire

# creating filter for LA

aberdeenshire <- scot_master %>% 
  filter(LA_Name == "Aberdeenshire") 

### what number of properties per grade

scot_aberdeenshire_summary <-  aberdeenshire %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_aberdeenshire_summary["la_name"] <- "Aberdeenshire"
scot_aberdeenshire_summary <- scot_aberdeenshire_summary[colnames(scot_aberdeenshire_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- aberdeenshire %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_aberdeenshire_summary <- scot_aberdeenshire_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- aberdeenshire %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_aberdeenshire_summary <- scot_aberdeenshire_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- aberdeenshire %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_aberdeenshire_summary <- scot_aberdeenshire_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- aberdeenshire %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_aberdeenshire_summary <- scot_aberdeenshire_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- aberdeenshire %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(aberdeenshire)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_aberdeenshire_summary <- scot_aberdeenshire_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- aberdeenshire %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(aberdeenshire)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_aberdeenshire_summary <- scot_aberdeenshire_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- aberdeenshire %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(aberdeenshire)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_aberdeenshire_summary <- scot_aberdeenshire_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- aberdeenshire %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_aberdeenshire_summary <- scot_aberdeenshire_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- aberdeenshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_aberdeenshire_summary <- scot_aberdeenshire_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- aberdeenshire %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_aberdeenshire_summary <- scot_aberdeenshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- aberdeenshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_aberdeenshire_summary <- scot_aberdeenshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)

### Angus

# creating filter for LA

angus <- scot_master %>% 
  filter(LA_Name == "Angus") 

### what number of properties per grade

scot_angus_summary <-  angus %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_angus_summary["la_name"] <- "Angus"
scot_angus_summary <- scot_angus_summary[colnames(scot_angus_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- angus %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_angus_summary <- scot_angus_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- angus %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_angus_summary <- scot_angus_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- angus %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_angus_summary <- scot_angus_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- angus %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_angus_summary <- scot_angus_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- angus %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(angus)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_angus_summary <- scot_angus_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- angus %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(angus)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_angus_summary <- scot_angus_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- angus %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(angus)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_angus_summary <- scot_angus_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- angus %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_angus_summary <- scot_angus_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- angus %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_angus_summary <- scot_angus_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- angus %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_angus_summary <- scot_angus_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- angus %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_angus_summary <- scot_angus_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)

### Argull and Bute
# creating filter for LA

Argyll_and_Bute <- scot_master %>% 
  filter(LA_Name == "Argyll & Bute") 

### what number of properties per grade

scot_argyll_and_bute_summary <-  Argyll_and_Bute %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_argyll_and_bute_summary["la_name"] <- "Argyll & Bute"
scot_argyll_and_bute_summary <- scot_argyll_and_bute_summary[colnames(scot_argyll_and_bute_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- Argyll_and_Bute %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_argyll_and_bute_summary <- scot_argyll_and_bute_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- Argyll_and_Bute %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_argyll_and_bute_summary <- scot_argyll_and_bute_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- Argyll_and_Bute %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_argyll_and_bute_summary <- scot_argyll_and_bute_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- Argyll_and_Bute %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_argyll_and_bute_summary <- scot_argyll_and_bute_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- Argyll_and_Bute %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(Argyll_and_Bute)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_argyll_and_bute_summary <- scot_argyll_and_bute_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- Argyll_and_Bute %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(Argyll_and_Bute)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_argyll_and_bute_summary <- scot_argyll_and_bute_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- Argyll_and_Bute %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(Argyll_and_Bute)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_argyll_and_bute_summary <- scot_argyll_and_bute_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- Argyll_and_Bute %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_argyll_and_bute_summary <- scot_argyll_and_bute_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- Argyll_and_Bute %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_argyll_and_bute_summary <- scot_argyll_and_bute_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- Argyll_and_Bute %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_argyll_and_bute_summary <- scot_argyll_and_bute_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- Argyll_and_Bute %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_argyll_and_bute_summary <- scot_argyll_and_bute_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)

### Clackmannanshire
# creating filter for LA

Clackmannanshire <- scot_master %>% 
  filter(LA_Name == "Clackmannanshire") 

### what number of properties per grade

scot_clackmannanshire_summary <-  Clackmannanshire %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_clackmannanshire_summary["la_name"] <- "Clackmannanshire"
scot_clackmannanshire_summary <- scot_clackmannanshire_summary[colnames(scot_clackmannanshire_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- Clackmannanshire %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_clackmannanshire_summary <- scot_clackmannanshire_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- Clackmannanshire %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_clackmannanshire_summary <- scot_clackmannanshire_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- Clackmannanshire %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_clackmannanshire_summary <- scot_clackmannanshire_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- Clackmannanshire %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_clackmannanshire_summary <- scot_clackmannanshire_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- Clackmannanshire %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(Clackmannanshire)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_clackmannanshire_summary <- scot_clackmannanshire_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- Clackmannanshire %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(Clackmannanshire)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_clackmannanshire_summary <- scot_clackmannanshire_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- Clackmannanshire %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(Clackmannanshire)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_clackmannanshire_summary <- scot_clackmannanshire_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- Clackmannanshire %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_clackmannanshire_summary <- scot_clackmannanshire_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- Clackmannanshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_clackmannanshire_summary <- scot_clackmannanshire_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- Clackmannanshire %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_clackmannanshire_summary <- scot_clackmannanshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- Clackmannanshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_clackmannanshire_summary <- scot_clackmannanshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)

### Dumfries and Galloway

# creating filter for LA

dumfries_and_galloway <- scot_master %>% 
  filter(LA_Name == "Dumfries & Galloway") 

### what number of properties per grade

scot_dumfries_and_galloway_summary <-  dumfries_and_galloway %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_dumfries_and_galloway_summary["la_name"] <- "Dumfries & Galloway"
scot_dumfries_and_galloway_summary <- scot_dumfries_and_galloway_summary[colnames(scot_dumfries_and_galloway_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- dumfries_and_galloway %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_dumfries_and_galloway_summary <- scot_dumfries_and_galloway_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- dumfries_and_galloway %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_dumfries_and_galloway_summary <- scot_dumfries_and_galloway_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- dumfries_and_galloway %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_dumfries_and_galloway_summary <- scot_dumfries_and_galloway_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- dumfries_and_galloway %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_dumfries_and_galloway_summary <- scot_dumfries_and_galloway_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- dumfries_and_galloway %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(dumfries_and_galloway)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_dumfries_and_galloway_summary <- scot_dumfries_and_galloway_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- dumfries_and_galloway %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(dumfries_and_galloway)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_dumfries_and_galloway_summary <- scot_dumfries_and_galloway_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- dumfries_and_galloway %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(dumfries_and_galloway)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_dumfries_and_galloway_summary <- scot_dumfries_and_galloway_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- dumfries_and_galloway %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_dumfries_and_galloway_summary <- scot_dumfries_and_galloway_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- dumfries_and_galloway %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_dumfries_and_galloway_summary <- scot_dumfries_and_galloway_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- dumfries_and_galloway %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_dumfries_and_galloway_summary <- scot_dumfries_and_galloway_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- dumfries_and_galloway %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_dumfries_and_galloway_summary <- scot_dumfries_and_galloway_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)

### Dundee City

# creating filter for LA

dundee_city <- scot_master %>% 
  filter(LA_Name == "Dundee City") 

### what number of properties per grade

scot_dundee_city_summary <-  dundee_city %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_dundee_city_summary["la_name"] <- "Dundee City"
scot_dundee_city_summary <- scot_dundee_city_summary[colnames(scot_dundee_city_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- dundee_city %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_dundee_city_summary <- scot_dundee_city_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- dundee_city %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_dundee_city_summary <- scot_dundee_city_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- dundee_city %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_dundee_city_summary <- scot_dundee_city_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- dundee_city %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_dundee_city_summary <- scot_dundee_city_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- dundee_city %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(dundee_city)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_dundee_city_summary <- scot_dundee_city_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- dundee_city %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(dundee_city)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_dundee_city_summary <- scot_dundee_city_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- dundee_city %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(dundee_city)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_dundee_city_summary <- scot_dundee_city_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- dundee_city %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_dundee_city_summary <- scot_dundee_city_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- dundee_city %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_dundee_city_summary <- scot_dundee_city_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- dundee_city %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_dundee_city_summary <- scot_dundee_city_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- dundee_city %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_dundee_city_summary <- scot_dundee_city_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)

### East Dunbartonshore

# creating filter for LA

east_dunbartonshire <- scot_master %>% 
  filter(LA_Name == "East Dunbartonshire") 

### what number of properties per grade

scot_east_dunbartonshire_summary <-  east_dunbartonshire %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_east_dunbartonshire_summary["la_name"] <- "East Dunbartonshire"
scot_east_dunbartonshire_summary <- scot_east_dunbartonshire_summary[colnames(scot_east_dunbartonshire_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- east_dunbartonshire %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_east_dunbartonshire_summary <- scot_east_dunbartonshire_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- east_dunbartonshire %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_east_dunbartonshire_summary <- scot_east_dunbartonshire_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- east_dunbartonshire %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_east_dunbartonshire_summary <- scot_east_dunbartonshire_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- east_dunbartonshire %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_east_dunbartonshire_summary <- scot_east_dunbartonshire_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- east_dunbartonshire %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_dunbartonshire)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_east_dunbartonshire_summary <- scot_east_dunbartonshire_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- east_dunbartonshire %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_dunbartonshire)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_east_dunbartonshire_summary <- scot_east_dunbartonshire_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- east_dunbartonshire %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_dunbartonshire)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_east_dunbartonshire_summary <- scot_east_dunbartonshire_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- east_dunbartonshire %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_east_dunbartonshire_summary <- scot_east_dunbartonshire_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- east_dunbartonshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_east_dunbartonshire_summary <- scot_east_dunbartonshire_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- east_dunbartonshire %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_east_dunbartonshire_summary <- scot_east_dunbartonshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- east_dunbartonshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_east_dunbartonshire_summary <- scot_east_dunbartonshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


###East Ayrshire

# creating filter for LA

east_ayrshire <- scot_master %>% 
  filter(LA_Name == "East Ayrshire") 

### what number of properties per grade

scot_east_ayrshire_summary <-  east_ayrshire %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_east_ayrshire_summary["la_name"] <- "East Ayrshire"
scot_east_ayrshire_summary <- scot_east_ayrshire_summary[colnames(scot_east_ayrshire_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- east_ayrshire %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_east_ayrshire_summary <- scot_east_ayrshire_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- east_ayrshire %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_east_ayrshire_summary <- scot_east_ayrshire_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- east_ayrshire %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_east_ayrshire_summary <- scot_east_ayrshire_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- east_ayrshire %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_east_ayrshire_summary <- scot_east_ayrshire_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- east_ayrshire %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_ayrshire)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_east_ayrshire_summary <- scot_east_ayrshire_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- east_ayrshire %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_ayrshire)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_east_ayrshire_summary <- scot_east_ayrshire_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- east_ayrshire %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_ayrshire)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_east_ayrshire_summary <- scot_east_ayrshire_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- east_ayrshire %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_east_ayrshire_summary <- scot_east_ayrshire_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- east_ayrshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_east_ayrshire_summary <- scot_east_ayrshire_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- east_ayrshire %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_east_ayrshire_summary <- scot_east_ayrshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- east_ayrshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_east_ayrshire_summary <- scot_east_ayrshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)

### East Lothian
# creating filter for LA

east_lothian <- scot_master %>% 
  filter(LA_Name == "East Lothian") 

### what number of properties per grade

scot_east_lothian_summary <-  east_lothian %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_east_lothian_summary["la_name"] <- "East Lothian"
scot_east_lothian_summary <- scot_east_lothian_summary[colnames(scot_east_lothian_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- east_lothian %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_east_lothian_summary <- scot_east_lothian_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- east_lothian %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_east_lothian_summary <- scot_east_lothian_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- east_lothian %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_east_lothian_summary <- scot_east_lothian_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- east_lothian %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_east_lothian_summary <- scot_east_lothian_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- east_lothian %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_lothian)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- east_lothian %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_lothian)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- east_lothian %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_lothian)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- east_lothian %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- east_lothian %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- east_lothian %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- east_lothian %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_east_lothian_summary <- scot_east_lothian_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)

### east renfrewshire


# creating filter for LA

east_renfrewshire <- scot_master %>% 
  filter(LA_Name == "East Renfrewshire") 

### what number of properties per grade

scot_east_renfrewshire_summary <-  east_renfrewshire %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_east_renfrewshire_summary["la_name"] <- "East Renfrewshire"
scot_east_renfrewshire_summary <- scot_east_renfrewshire_summary[colnames(scot_east_renfrewshire_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- east_renfrewshire %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_east_renfrewshire_summary <- scot_east_renfrewshire_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- east_renfrewshire %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_east_renfrewshire_summary <- scot_east_renfrewshire_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- east_renfrewshire %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_east_renfrewshire_summary <- scot_east_renfrewshire_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- east_renfrewshire %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_east_renfrewshire_summary <- scot_east_renfrewshire_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- east_renfrewshire %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_renfrewshire)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_east_renfrewshire_summary <- scot_east_renfrewshire_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- east_renfrewshire %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_renfrewshire)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_east_renfrewshire_summary <- scot_east_renfrewshire_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- east_renfrewshire %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(east_renfrewshire)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_east_renfrewshire_summary <- scot_east_renfrewshire_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- east_renfrewshire %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_east_renfrewshire_summary <- scot_east_renfrewshire_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- east_renfrewshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_east_renfrewshire_summary <- scot_east_renfrewshire_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- east_renfrewshire %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_east_renfrewshire_summary <- scot_east_renfrewshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- east_renfrewshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_east_renfrewshire_summary <- scot_east_renfrewshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)

#### Edinburgh
# creating filter for LA

edinburgh_city_of <- scot_master %>% 
  filter(LA_Name == "Edinburgh, City of") 

### what number of properties per grade

scot_edinburgh_city_of_summary <-  edinburgh_city_of %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_edinburgh_city_of_summary["la_name"] <- "Edinburgh, City of"
scot_edinburgh_city_of_summary <- scot_edinburgh_city_of_summary[colnames(scot_edinburgh_city_of_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- edinburgh_city_of %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_edinburgh_city_of_summary <- scot_edinburgh_city_of_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- edinburgh_city_of %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_edinburgh_city_of_summary <- scot_edinburgh_city_of_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- edinburgh_city_of %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_edinburgh_city_of_summary <- scot_edinburgh_city_of_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- edinburgh_city_of %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_edinburgh_city_of_summary <- scot_edinburgh_city_of_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- edinburgh_city_of %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(edinburgh_city_of)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_edinburgh_city_of_summary <- scot_edinburgh_city_of_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- edinburgh_city_of %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(edinburgh_city_of)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_edinburgh_city_of_summary <- scot_edinburgh_city_of_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- edinburgh_city_of %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(edinburgh_city_of)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_edinburgh_city_of_summary <- scot_edinburgh_city_of_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- edinburgh_city_of %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_edinburgh_city_of_summary <- scot_edinburgh_city_of_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- edinburgh_city_of %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_edinburgh_city_of_summary <- scot_edinburgh_city_of_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- edinburgh_city_of %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_edinburgh_city_of_summary <- scot_edinburgh_city_of_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- edinburgh_city_of %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_edinburgh_city_of_summary <- scot_edinburgh_city_of_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### Eilean Siar
# creating filter for LA

eilean_siar <- scot_master %>% 
  filter(LA_Name == "Eilean Siar") 

### what number of properties per grade

scot_eilean_siar_summary <-  eilean_siar %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_eilean_siar_summary["la_name"] <- "Eilean Siar"
scot_eilean_siar_summary <- scot_eilean_siar_summary[colnames(scot_eilean_siar_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- eilean_siar %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_eilean_siar_summary <- scot_eilean_siar_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- eilean_siar %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_eilean_siar_summary <- scot_eilean_siar_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- eilean_siar %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_eilean_siar_summary <- scot_eilean_siar_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- eilean_siar %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_eilean_siar_summary <- scot_eilean_siar_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- eilean_siar %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(eilean_siar)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_eilean_siar_summary <- scot_eilean_siar_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- eilean_siar %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(eilean_siar)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_eilean_siar_summary <- scot_eilean_siar_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- eilean_siar %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(eilean_siar)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_eilean_siar_summary <- scot_eilean_siar_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- eilean_siar %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_eilean_siar_summary <- scot_eilean_siar_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- eilean_siar %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_eilean_siar_summary <- scot_eilean_siar_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- eilean_siar %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_eilean_siar_summary <- scot_eilean_siar_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- eilean_siar %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_eilean_siar_summary <- scot_eilean_siar_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### falkirk
# creating filter for LA

falkirk <- scot_master %>% 
  filter(LA_Name == "Falkirk") 

### what number of properties per grade

scot_falkirk_summary <-  falkirk %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_falkirk_summary["la_name"] <- "Falkirk"
scot_falkirk_summary <- scot_falkirk_summary[colnames(scot_falkirk_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- falkirk %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_falkirk_summary <- scot_falkirk_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- falkirk %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_falkirk_summary <- scot_falkirk_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- falkirk %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_falkirk_summary <- scot_falkirk_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- falkirk %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_falkirk_summary <- scot_falkirk_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- falkirk %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(falkirk)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_falkirk_summary <- scot_falkirk_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- falkirk %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(falkirk)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_falkirk_summary <- scot_falkirk_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- falkirk %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(falkirk)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_falkirk_summary <- scot_falkirk_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- falkirk %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_falkirk_summary <- scot_falkirk_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- falkirk %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_falkirk_summary <- scot_falkirk_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- falkirk %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_falkirk_summary <- scot_falkirk_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- falkirk %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_falkirk_summary <- scot_falkirk_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### FIFE

# creating filter for LA

fife <- scot_master %>% 
  filter(LA_Name == "Fife") 

### what number of properties per grade

scot_fife_summary <-  fife %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_fife_summary["la_name"] <- "Fife"
scot_fife_summary <- scot_fife_summary[colnames(scot_fife_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- fife %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_fife_summary <- scot_fife_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- fife %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_fife_summary <- scot_fife_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- fife %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_fife_summary <- scot_fife_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- fife %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_fife_summary <- scot_fife_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- fife %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(fife)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_fife_summary <- scot_fife_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- fife %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(fife)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_fife_summary <- scot_fife_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- fife %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(fife)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_fife_summary <- scot_fife_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- fife %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_fife_summary <- scot_fife_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- fife %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_fife_summary <- scot_fife_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- fife %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_fife_summary <- scot_fife_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- fife %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_fife_summary <- scot_fife_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### Glasgow City
# creating filter for LA

glasgow_city <- scot_master %>% 
  filter(LA_Name == "Glasgow City") 

### what number of properties per grade

scot_glasgow_city_summary <-  glasgow_city %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_glasgow_city_summary["la_name"] <- "Glasgow City"
scot_glasgow_city_summary <- scot_glasgow_city_summary[colnames(scot_glasgow_city_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- glasgow_city %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_glasgow_city_summary <- scot_glasgow_city_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- glasgow_city %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_glasgow_city_summary <- scot_glasgow_city_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- glasgow_city %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_glasgow_city_summary <- scot_glasgow_city_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- glasgow_city %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_glasgow_city_summary <- scot_glasgow_city_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- glasgow_city %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(glasgow_city)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_glasgow_city_summary <- scot_glasgow_city_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- glasgow_city %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(glasgow_city)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_glasgow_city_summary <- scot_glasgow_city_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- glasgow_city %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(glasgow_city)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_glasgow_city_summary <- scot_glasgow_city_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- glasgow_city %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_glasgow_city_summary <- scot_glasgow_city_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- glasgow_city %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_glasgow_city_summary <- scot_glasgow_city_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- glasgow_city %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_glasgow_city_summary <- scot_glasgow_city_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- glasgow_city %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_glasgow_city_summary <- scot_glasgow_city_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)



### Highland

# creating filter for LA

highland <- scot_master %>% 
  filter(LA_Name == "Highland") 

### what number of properties per grade

scot_highland_summary <-  highland %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_highland_summary["la_name"] <- "Highland"
scot_highland_summary <- scot_highland_summary[colnames(scot_highland_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- highland %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_highland_summary <- scot_highland_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- highland %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_highland_summary <- scot_highland_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- highland %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_highland_summary <- scot_highland_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- highland %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_highland_summary <- scot_highland_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- highland %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(highland)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_highland_summary <- scot_highland_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- highland %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(highland)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_highland_summary <- scot_highland_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- highland %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(highland)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_highland_summary <- scot_highland_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- highland %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_highland_summary <- scot_highland_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- highland %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_highland_summary <- scot_highland_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- highland %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_highland_summary <- scot_highland_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- highland %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_highland_summary <- scot_highland_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### inverclyde
# creating filter for LA

inverclyde <- scot_master %>% 
  filter(LA_Name == "Inverclyde") 

### what number of properties per grade

scot_inverclyde_summary <-  inverclyde %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_inverclyde_summary["la_name"] <- "Inverclyde"
scot_inverclyde_summary <- scot_inverclyde_summary[colnames(scot_inverclyde_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- inverclyde %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_inverclyde_summary <- scot_inverclyde_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- inverclyde %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_inverclyde_summary <- scot_inverclyde_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- inverclyde %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_inverclyde_summary <- scot_inverclyde_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- inverclyde %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_inverclyde_summary <- scot_inverclyde_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- inverclyde %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(inverclyde)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_inverclyde_summary <- scot_inverclyde_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- inverclyde %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(inverclyde)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_inverclyde_summary <- scot_inverclyde_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- inverclyde %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(inverclyde)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_inverclyde_summary <- scot_inverclyde_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- inverclyde %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_inverclyde_summary <- scot_inverclyde_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- inverclyde %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_inverclyde_summary <- scot_inverclyde_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- inverclyde %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_inverclyde_summary <- scot_inverclyde_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- inverclyde %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_inverclyde_summary <- scot_inverclyde_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


#### Midlothian
# creating filter for LA

midlothian <- scot_master %>% 
  filter(LA_Name == "Midlothian") 

### what number of properties per grade

scot_midlothian_summary <-  midlothian %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_midlothian_summary["la_name"] <- "Midlothian"
scot_midlothian_summary <- scot_midlothian_summary[colnames(scot_midlothian_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- midlothian %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_midlothian_summary <- scot_midlothian_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- midlothian %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_midlothian_summary <- scot_midlothian_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- midlothian %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_midlothian_summary <- scot_midlothian_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- midlothian %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_midlothian_summary <- scot_midlothian_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- midlothian %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(midlothian)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_midlothian_summary <- scot_midlothian_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- midlothian %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(midlothian)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_midlothian_summary <- scot_midlothian_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- midlothian %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(midlothian)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_midlothian_summary <- scot_midlothian_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- midlothian %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_midlothian_summary <- scot_midlothian_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- midlothian %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_midlothian_summary <- scot_midlothian_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- midlothian %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_midlothian_summary <- scot_midlothian_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- midlothian %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_midlothian_summary <- scot_midlothian_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)

### Moray
# creating filter for LA

moray <- scot_master %>% 
  filter(LA_Name == "Moray") 

### what number of properties per grade

scot_moray_summary <-  moray %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_moray_summary["la_name"] <- "Moray"
scot_moray_summary <- scot_moray_summary[colnames(scot_moray_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- moray %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_moray_summary <- scot_moray_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- moray %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_moray_summary <- scot_moray_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- moray %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_moray_summary <- scot_moray_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- moray %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_moray_summary <- scot_moray_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- moray %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(moray)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_moray_summary <- scot_moray_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- moray %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(moray)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_moray_summary <- scot_moray_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- moray %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(moray)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_moray_summary <- scot_moray_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- moray %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_moray_summary <- scot_moray_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- moray %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_moray_summary <- scot_moray_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- moray %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_moray_summary <- scot_moray_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- moray %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_moray_summary <- scot_moray_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### North Ayrshire

# creating filter for LA

north_ayrshire <- scot_master %>% 
  filter(LA_Name == "North Ayrshire") 

### what number of properties per grade

scot_north_ayrshire_summary <-  north_ayrshire %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_north_ayrshire_summary["la_name"] <- "North Ayrshire"
scot_north_ayrshire_summary <- scot_north_ayrshire_summary[colnames(scot_north_ayrshire_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- north_ayrshire %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_north_ayrshire_summary <- scot_north_ayrshire_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- north_ayrshire %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_north_ayrshire_summary <- scot_north_ayrshire_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- north_ayrshire %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_north_ayrshire_summary <- scot_north_ayrshire_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- north_ayrshire %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_north_ayrshire_summary <- scot_north_ayrshire_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- north_ayrshire %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(north_ayrshire)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_north_ayrshire_summary <- scot_north_ayrshire_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- north_ayrshire %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(north_ayrshire)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_north_ayrshire_summary <- scot_north_ayrshire_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- north_ayrshire %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(north_ayrshire)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_north_ayrshire_summary <- scot_north_ayrshire_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- north_ayrshire %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_north_ayrshire_summary <- scot_north_ayrshire_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- north_ayrshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_north_ayrshire_summary <- scot_north_ayrshire_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- north_ayrshire %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_north_ayrshire_summary <- scot_north_ayrshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- north_ayrshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_north_ayrshire_summary <- scot_north_ayrshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### North Lanarkshire
# creating filter for LA

north_lanarkshire <- scot_master %>% 
  filter(LA_Name == "North Lanarkshire") 

### what number of properties per grade

scot_north_lanarkshire_summary <-  north_lanarkshire %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_north_lanarkshire_summary["la_name"] <- "North Lanarkshire"
scot_north_lanarkshire_summary <- scot_north_lanarkshire_summary[colnames(scot_north_lanarkshire_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- north_lanarkshire %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_north_lanarkshire_summary <- scot_north_lanarkshire_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- north_lanarkshire %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_north_lanarkshire_summary <- scot_north_lanarkshire_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- north_lanarkshire %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_north_lanarkshire_summary <- scot_north_lanarkshire_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- north_lanarkshire %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_north_lanarkshire_summary <- scot_north_lanarkshire_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- north_lanarkshire %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(north_lanarkshire)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_north_lanarkshire_summary <- scot_north_lanarkshire_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- north_lanarkshire %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(north_lanarkshire)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_north_lanarkshire_summary <- scot_north_lanarkshire_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- north_lanarkshire %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(north_lanarkshire)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_north_lanarkshire_summary <- scot_north_lanarkshire_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- north_lanarkshire %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_north_lanarkshire_summary <- scot_north_lanarkshire_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- north_lanarkshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_north_lanarkshire_summary <- scot_north_lanarkshire_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- north_lanarkshire %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_north_lanarkshire_summary <- scot_north_lanarkshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- north_lanarkshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_north_lanarkshire_summary <- scot_north_lanarkshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### Orkney Islands
# creating filter for LA

orkney_islands <- scot_master %>% 
  filter(LA_Name == "Orkney Islands") 

### what number of properties per grade

scot_orkney_islands_summary <-  orkney_islands %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_orkney_islands_summary["la_name"] <- "Orkney Islands"
scot_orkney_islands_summary <- scot_orkney_islands_summary[colnames(scot_orkney_islands_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- orkney_islands %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_orkney_islands_summary <- scot_orkney_islands_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- orkney_islands %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_orkney_islands_summary <- scot_orkney_islands_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- orkney_islands %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_orkney_islands_summary <- scot_orkney_islands_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- orkney_islands %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_orkney_islands_summary <- scot_orkney_islands_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- orkney_islands %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(orkney_islands)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_orkney_islands_summary <- scot_orkney_islands_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- orkney_islands %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(orkney_islands)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_orkney_islands_summary <- scot_orkney_islands_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- orkney_islands %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(orkney_islands)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_orkney_islands_summary <- scot_orkney_islands_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- orkney_islands %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_orkney_islands_summary <- scot_orkney_islands_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- orkney_islands %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_orkney_islands_summary <- scot_orkney_islands_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- orkney_islands %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_orkney_islands_summary <- scot_orkney_islands_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- orkney_islands %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_orkney_islands_summary <- scot_orkney_islands_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### Perth & Kinross
# creating filter for LA

perth_and_kinross <- scot_master %>% 
  filter(LA_Name == "Perth & Kinross") 

### what number of properties per grade

scot_perth_and_kinross_summary <-  perth_and_kinross %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_perth_and_kinross_summary["la_name"] <- "Perth & Kinross"
scot_perth_and_kinross_summary <- scot_perth_and_kinross_summary[colnames(scot_perth_and_kinross_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- perth_and_kinross %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_perth_and_kinross_summary <- scot_perth_and_kinross_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- perth_and_kinross %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_perth_and_kinross_summary <- scot_perth_and_kinross_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- perth_and_kinross %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_perth_and_kinross_summary <- scot_perth_and_kinross_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- perth_and_kinross %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_perth_and_kinross_summary <- scot_perth_and_kinross_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- perth_and_kinross %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(perth_and_kinross)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_perth_and_kinross_summary <- scot_perth_and_kinross_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- perth_and_kinross %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(perth_and_kinross)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_perth_and_kinross_summary <- scot_perth_and_kinross_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- perth_and_kinross %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(perth_and_kinross)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_perth_and_kinross_summary <- scot_perth_and_kinross_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- perth_and_kinross %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_perth_and_kinross_summary <- scot_perth_and_kinross_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- perth_and_kinross %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_perth_and_kinross_summary <- scot_perth_and_kinross_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- perth_and_kinross %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_perth_and_kinross_summary <- scot_perth_and_kinross_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- perth_and_kinross %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_perth_and_kinross_summary <- scot_perth_and_kinross_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### Renfrewshire
# creating filter for LA

renfrewshire <- scot_master %>% 
  filter(LA_Name == "Renfrewshire") 

### what number of properties per grade

scot_renfrewshire_summary <-  renfrewshire %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_renfrewshire_summary["la_name"] <- "Renfrewshire"
scot_renfrewshire_summary <- scot_renfrewshire_summary[colnames(scot_renfrewshire_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- renfrewshire %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_renfrewshire_summary <- scot_renfrewshire_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- renfrewshire %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_renfrewshire_summary <- scot_renfrewshire_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- renfrewshire %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_renfrewshire_summary <- scot_renfrewshire_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- renfrewshire %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_renfrewshire_summary <- scot_renfrewshire_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- renfrewshire %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(renfrewshire)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_renfrewshire_summary <- scot_renfrewshire_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- renfrewshire %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(renfrewshire)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_renfrewshire_summary <- scot_renfrewshire_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- renfrewshire %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(renfrewshire)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_renfrewshire_summary <- scot_renfrewshire_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- renfrewshire %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_renfrewshire_summary <- scot_renfrewshire_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- renfrewshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_renfrewshire_summary <- scot_renfrewshire_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- renfrewshire %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_renfrewshire_summary <- scot_renfrewshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- renfrewshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_renfrewshire_summary <- scot_renfrewshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### Scottish Borders

# creating filter for LA

scottish_borders <- scot_master %>% 
  filter(LA_Name == "Scottish Borders") 

### what number of properties per grade

scot_scottish_borders_summary <-  scottish_borders %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_scottish_borders_summary["la_name"] <- "Scottish Borders"
scot_scottish_borders_summary <- scot_scottish_borders_summary[colnames(scot_scottish_borders_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- scottish_borders %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_scottish_borders_summary <- scot_scottish_borders_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- scottish_borders %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_scottish_borders_summary <- scot_scottish_borders_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- scottish_borders %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_scottish_borders_summary <- scot_scottish_borders_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- scottish_borders %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_scottish_borders_summary <- scot_scottish_borders_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- scottish_borders %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(scottish_borders)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_scottish_borders_summary <- scot_scottish_borders_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- scottish_borders %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(scottish_borders)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_scottish_borders_summary <- scot_scottish_borders_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- scottish_borders %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(scottish_borders)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_scottish_borders_summary <- scot_scottish_borders_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- scottish_borders %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_scottish_borders_summary <- scot_scottish_borders_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- scottish_borders %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_scottish_borders_summary <- scot_scottish_borders_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- scottish_borders %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_scottish_borders_summary <- scot_scottish_borders_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- scottish_borders %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_scottish_borders_summary <- scot_scottish_borders_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### Shetland Islands
# creating filter for LA

shetland_islands <- scot_master %>% 
  filter(LA_Name == "Shetland Islands") 

### what number of properties per grade

scot_shetland_islands_summary <-  shetland_islands %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_shetland_islands_summary["la_name"] <- "Shetland Islands"
scot_shetland_islands_summary <- scot_shetland_islands_summary[colnames(scot_shetland_islands_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- shetland_islands %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_shetland_islands_summary <- scot_shetland_islands_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- shetland_islands %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_shetland_islands_summary <- scot_shetland_islands_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- shetland_islands %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_shetland_islands_summary <- scot_shetland_islands_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- shetland_islands %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_shetland_islands_summary <- scot_shetland_islands_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- shetland_islands %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(shetland_islands)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_shetland_islands_summary <- scot_shetland_islands_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- shetland_islands %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(shetland_islands)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_shetland_islands_summary <- scot_shetland_islands_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- shetland_islands %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(shetland_islands)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_shetland_islands_summary <- scot_shetland_islands_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- shetland_islands %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_shetland_islands_summary <- scot_shetland_islands_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- shetland_islands %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_shetland_islands_summary <- scot_shetland_islands_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- shetland_islands %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_shetland_islands_summary <- scot_shetland_islands_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- shetland_islands %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_shetland_islands_summary <- scot_shetland_islands_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### south ayrshire
# creating filter for LA

south_ayrshire <- scot_master %>% 
  filter(LA_Name == "South Ayrshire") 

### what number of properties per grade

scot_south_ayrshire_summary <-  south_ayrshire %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_south_ayrshire_summary["la_name"] <- "South Ayrshire"
scot_south_ayrshire_summary <- scot_south_ayrshire_summary[colnames(scot_south_ayrshire_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- south_ayrshire %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_south_ayrshire_summary <- scot_south_ayrshire_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- south_ayrshire %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_south_ayrshire_summary <- scot_south_ayrshire_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- south_ayrshire %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_south_ayrshire_summary <- scot_south_ayrshire_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- south_ayrshire %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_south_ayrshire_summary <- scot_south_ayrshire_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- south_ayrshire %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(south_ayrshire)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_south_ayrshire_summary <- scot_south_ayrshire_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- south_ayrshire %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(south_ayrshire)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_south_ayrshire_summary <- scot_south_ayrshire_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- south_ayrshire %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(south_ayrshire)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_south_ayrshire_summary <- scot_south_ayrshire_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- south_ayrshire %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_south_ayrshire_summary <- scot_south_ayrshire_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- south_ayrshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_south_ayrshire_summary <- scot_south_ayrshire_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- south_ayrshire %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_south_ayrshire_summary <- scot_south_ayrshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- south_ayrshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_south_ayrshire_summary <- scot_south_ayrshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### south lanarkshire
# creating filter for LA

south_lanarkshire <- scot_master %>% 
  filter(LA_Name == "South Lanarkshire") 

### what number of properties per grade

scot_south_lanarkshire_summary <-  south_ayrshire %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_south_lanarkshire_summary["la_name"] <- "South Lanarkshire"
scot_south_lanarkshire_summary <- scot_south_lanarkshire_summary[colnames(scot_south_lanarkshire_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- south_ayrshire %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_south_lanarkshire_summary <- scot_south_lanarkshire_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- south_ayrshire %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_south_lanarkshire_summary <- scot_south_lanarkshire_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- south_ayrshire %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_south_lanarkshire_summary <- scot_south_lanarkshire_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- south_ayrshire %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_south_lanarkshire_summary <- scot_south_lanarkshire_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- south_ayrshire %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(south_ayrshire)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_south_lanarkshire_summary <- scot_south_lanarkshire_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- south_ayrshire %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(south_ayrshire)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_south_lanarkshire_summary <- scot_south_lanarkshire_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- south_ayrshire %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(south_ayrshire)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_south_lanarkshire_summary <- scot_south_lanarkshire_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- south_ayrshire %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_south_lanarkshire_summary <- scot_south_lanarkshire_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- south_ayrshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_south_lanarkshire_summary <- scot_south_lanarkshire_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- south_ayrshire %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_south_lanarkshire_summary <- scot_south_lanarkshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- south_ayrshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_south_lanarkshire_summary <- scot_south_lanarkshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### stirling
# creating filter for LA

stirling <- scot_master %>% 
  filter(LA_Name == "Stirling") 

### what number of properties per grade

scot_stirling_summary <-  stirling %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_stirling_summary["la_name"] <- "Stirling"
scot_stirling_summary <- scot_stirling_summary[colnames(scot_stirling_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- stirling %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_stirling_summary <- scot_stirling_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- stirling %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_stirling_summary <- scot_stirling_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- stirling %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_stirling_summary <- scot_stirling_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- stirling %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_stirling_summary <- scot_stirling_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- stirling %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(stirling)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_stirling_summary <- scot_stirling_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- stirling %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(stirling)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_stirling_summary <- scot_stirling_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- stirling %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(stirling)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_stirling_summary <- scot_stirling_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- stirling %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_stirling_summary <- scot_stirling_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- stirling %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_stirling_summary <- scot_stirling_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- stirling %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_stirling_summary <- scot_stirling_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- stirling %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_stirling_summary <- scot_stirling_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)


### west dunbartonshire

# creating filter for LA

west_dunbartonshire <- scot_master %>% 
  filter(LA_Name == "West Dunbartonshire") 

### what number of properties per grade

scot_west_dunbartonshire_summary <-  west_dunbartonshire %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_west_dunbartonshire_summary["la_name"] <- "West Dunbartonshire"
scot_west_dunbartonshire_summary <- scot_west_dunbartonshire_summary[colnames(scot_west_dunbartonshire_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- west_dunbartonshire %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_west_dunbartonshire_summary <- scot_west_dunbartonshire_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- west_dunbartonshire %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_west_dunbartonshire_summary <- scot_west_dunbartonshire_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- west_dunbartonshire %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_west_dunbartonshire_summary <- scot_west_dunbartonshire_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- west_dunbartonshire %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_west_dunbartonshire_summary <- scot_west_dunbartonshire_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- west_dunbartonshire %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(west_dunbartonshire)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_west_dunbartonshire_summary <- scot_west_dunbartonshire_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- west_dunbartonshire %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(west_dunbartonshire)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_west_dunbartonshire_summary <- scot_west_dunbartonshire_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- west_dunbartonshire %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(west_dunbartonshire)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_west_dunbartonshire_summary <- scot_west_dunbartonshire_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)
### Possible average savings in lighting

avg_pot_lighting_savings <- west_dunbartonshire %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_west_dunbartonshire_summary <- scot_west_dunbartonshire_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- west_dunbartonshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_west_dunbartonshire_summary <- scot_west_dunbartonshire_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- west_dunbartonshire %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_west_dunbartonshire_summary <- scot_west_dunbartonshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- west_dunbartonshire %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_west_dunbartonshire_summary <- scot_west_dunbartonshire_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)

### west lothian
# creating filter for LA

west_lothian <- scot_master %>% 
  filter(LA_Name == "West Lothian") 

### what number of properties per grade

scot_west_lothian_summary <-  west_lothian %>%
  group_by(current_energy_efficiency_rating_band) %>% 
  summarise(number_of_obs=n()) %>% 
  spread(current_energy_efficiency_rating_band, number_of_obs)

scot_west_lothian_summary["la_name"] <- "West Lothian"
scot_west_lothian_summary <- scot_west_lothian_summary[colnames(scot_west_lothian_summary)[c(8,1:7)]]


# what is the average energy efficiency of the LA?

avg_scot_ee <- west_lothian %>%  
  summarise(avg_scot_ee = mean(current_energy_efficiency_rating))

x <- as.character(avg_scot_ee)

y <- if (x >= 92) {
  print("A")
} else if (x >=81) {
  print("B")
} else if (x >=69) {
  print("C")
} else if (x >=55) {
  print("D")
} else if (x >=39) {
  print("E")
} else if (x >=21) {
  print("F")
} else {
  print("G")}

avg_scot_ee["Avg_grade"] <- y


scot_west_lothian_summary <- scot_west_lothian_summary %>%
  mutate(avg_grade = avg_scot_ee$Avg_grade)
### What is the average upgrade in terms Energy Rating?


avg_upgrade <- west_lothian %>%
  summarise(avg_upgrade=mean(potential_energy_efficiency_rating-current_energy_efficiency_rating))

z <- as.character(as.numeric(x)+avg_upgrade)

potential_increase <- if (z >= 92) {
  print("A")
} else if (z >=81) {
  print("B")
} else if (z >=69) {
  print("C")
} else if (z >=55) {
  print("D")
} else if (z >=39) {
  print("E")
} else if (z >=21) {
  print("F")
} else {
  print("G")}

avg_upgrade["Avg_upgrade"] <- potential_increase

scot_west_lothian_summary <- scot_west_lothian_summary %>%
  mutate(potential_increase = avg_upgrade$Avg_upgrade)


### What is the average CO2 emission for homes?
avg_co2 <- west_lothian %>% 
  summarise(avg_co2 = mean(current_emissions_t_co2_yr))

scot_west_lothian_summary <- scot_west_lothian_summary %>%
  mutate(avg_co2 = avg_co2$avg_co2)
### Average C02 emissions Potential 

avg_co2_pot <- west_lothian %>% 
  summarise(avg_co2_pot = mean(potential_reduction_in_emissions_t_co2_yr))

scot_west_lothian_summary <- scot_west_lothian_summary %>%
  mutate(avg_co2_pot = avg_co2_pot$avg_co2_pot)
### What number of properties have double glazing?

dbl_glazing <- west_lothian %>% 
  filter(str_detect(window_summary, "Fully double glazed")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(west_lothian)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

scot_west_lothian_summary <- scot_west_lothian_summary %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roof <- west_lothian %>% 
  filter(str_detect(roof_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(west_lothian)))*100) %>% 
  rename("No_roof_insulation" = total, "Percentage_of_total" =percentage) 

scot_west_lothian_summary <- scot_west_lothian_summary %>% 
  mutate(no_roof$No_roof_insulation, no_roof$Percentage_of_total)

### What number of properties have no wall insulation ?

no_wall <- west_lothian %>% 
  filter(str_detect(wall_summary, "no insulation")) %>% 
  summarise(total=n(), percentage=(n()/(nrow(west_lothian)))*100) %>% 
  rename("No_wall_insulation" = total, "Percentage_of_total" =percentage)

scot_west_lothian_summary <- scot_west_lothian_summary %>% 
  mutate(no_wall$No_wall_insulation, no_wall$Percentage_of_total)

### Possible average savings in lighting

avg_pot_lighting_savings <- west_lothian %>%
  summarise(avg_pot_lighting_savings_three_yrs=mean(current_lighting_costs_over_3_years-potential_lighting_costs_over_3_years))

scot_west_lothian_summary <- scot_west_lothian_summary %>% 
  mutate(avg_pot_lighting_savings$avg_pot_lighting_savings_three_yrs)
### Possible average savings in  heating
avg_pot_heating_savings <- west_lothian %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(current_heating_costs_over_3_years-potential_heating_costs_over_3_years))

scot_west_lothian_summary <- scot_west_lothian_summary %>% 
  mutate(avg_pot_heating_savings$avg_pot_heating_savings_three_yrs)
### Possible average savings in Hot water

avg_pot_hot_water_savings <- west_lothian %>%
  summarise(avg_pot_hot_water_savings_three_yrs=mean(current_hot_water_costs_over_3_years-potential_hot_water_costs_over_3_years))

scot_west_lothian_summary <- scot_west_lothian_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)
### Total potential savings over three years

avg_total_pot_savings <- west_lothian %>%
  summarise(avg_pot_heating_savings_three_yrs=mean(total_potential_energy_cost_after_3_years))

scot_west_lothian_summary <- scot_west_lothian_summary %>% 
  mutate(avg_pot_hot_water_savings$avg_pot_hot_water_savings_three_yrs)



# produce master summary sheet
sum_master <- do.call(rbind, lapply( ls(patt="_summary"), get) )
write_csv(sum_master, "sum_master.csv")
