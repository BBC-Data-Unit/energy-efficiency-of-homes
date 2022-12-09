Poole <- read.csv('certificates.csv')




### What is the average property grade
Average_grade <- Poole %>% 
  summarise(Avg_ee = mean(CURRENT_ENERGY_EFFICIENCY))

x <- as.character(Average_grade$Avg_ee)

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

Average_grade["Avg_grade"] <- y


### What is the average upgrade in terms Energy Rating?

avg_upgrade <- Poole %>%
  summarise(ee_potential_increase=mean(POTENTIAL_ENERGY_EFFICIENCY-CURRENT_ENERGY_EFFICIENCY))

z <- as.character(as.numeric(x)+avg_upgrade$ee_potential_increase)

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

average <- Average_grade %>% 
  mutate(avg_upgrade$ee_potential_increase,avg_upgrade$Avg_upgrade)

average["LOCAL_AUTHORITY"] <- "Poole"
average <- average[colnames(average)[c(5,1:4)]]

### How many properties are in Grade A
Energy_scoreA <- Poole %>% 
  filter(CURRENT_ENERGY_RATING == "A") %>% 
  group_by(CURRENT_ENERGY_RATING) %>% 
  summarise(No_GradeA =n()) 

Energy_scoreA$CURRENT_ENERGY_RATING <- NULL

#average <- average %>%

#select(-("Number Properties Grade A")) %>% 
#rename("number_grade_A"=Energy_scoreA$No_GradeA)
#names(average)

average["Number Properties Grade A"] <- Energy_scoreA
average <- average[, c(1, 6, 2, 3, 4, 5)]

### How many properties are in Grade B
Energy_scoreB <- Poole %>% 
  filter(CURRENT_ENERGY_RATING == "B") %>% 
  group_by(CURRENT_ENERGY_RATING) %>% 
  summarise(No_GradeB =n()) 

Energy_scoreB$CURRENT_ENERGY_RATING <- NULL

#average <- average %>%

#select(-("Number Properties Grade A")) %>% 
#rename("number_grade_A"=Energy_scoreA$No_GradeA)
#names(average)

average["Number Properties Grade B"] <- Energy_scoreB
average <- average[, c(1, 2, 7, 3, 4, 5, 6)]
### How many properties are in Grade G
Energy_scoreG <- Poole %>% 
  filter(CURRENT_ENERGY_RATING == "G") %>% 
  group_by(CURRENT_ENERGY_RATING) %>% 
  summarise(No_GradeG =n())

Energy_scoreG$CURRENT_ENERGY_RATING <- NULL

average["Number Properties Grade G"] <- Energy_scoreG
average <- average[, c(1, 2, 3, 8, 4, 5, 6, 7)]
average

### How many properties are in Grade C
Energy_scoreC <- Poole %>% 
  filter(CURRENT_ENERGY_RATING == "C") %>% 
  group_by(CURRENT_ENERGY_RATING) %>% 
  summarise(No_GradeC = n())

Energy_scoreC$CURRENT_ENERGY_RATING <- NULL

average["Number Properties Grade C"] <- Energy_scoreC
average <- average[, c(1, 2, 3, 9, 4, 5, 6, 7, 8)]
average

### What is the average CO2 emission for homes?

Average_CO2_Emissions <- Poole %>% 
  summarise(Avg_CO2 = mean(CO2_EMISSIONS_CURRENT))
average["Average CO2 Emissions"] <- Average_CO2_Emissions
average

### Average C02 emissions Potential 
avg_CO2_empot <- Poole %>% 
  summarise(avg_CO2_empot = mean(CO2_EMISSIONS_POTENTIAL))
average["Average CO2 Emissions Potential"] <- avg_CO2_empot
average <- average [, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 22, 11:21)]

### What number of properties have double glazing?

dbl_glazing <- Poole %>% 
  filter(Windows == "Fully double glazed") %>%
  group_by(WINDOWS_DESCRIPTION) %>% 
  summarise(total=n(), percentage=(n()/(nrow(Poole)))*100) %>% 
  rename("Number_of_Double_Glazed" = total, "Percentage_of_total" =percentage)

average <- average %>% 
  mutate(dbl_glazing$Number_of_Double_Glazed, dbl_glazing$Percentage_of_total)

### What number of properties have no roof insulation ?

no_roofin <- Poole %>% 
  filter(str_detect(ROOF_DESCRIPTION, "no insulation")) %>%  
  summarise(total_no_roof_insulation=n(), pct_no_roof_insulation = (n()/(nrow(Poole))) * 100) 

average <- average %>% 
  mutate(no_roofin$total_no_roof_insulation, no_roofin$pct_no_roof_insulation)



### What is the average roof energy efficiency rating?


x <- Poole$ROOF_ENERGY_EFF

f <- function(x){
  dd <- unique(x)
  dd[which.max(tabulate(match(x,dd)))]
}

roof_ee <- f(Poole$ROOF_ENERGY_EFF)

average["Most Common Roof EE Rating"] <- roof_ee


### what is the average wall energy efficiency rating?

w <- Poole$WALLS_ENERGY_EFF

f2 <- function(w){
  dd <- unique(w)
  dd[which.max(tabulate(match(w,dd)))]
}

walls_ee <- f2(Poole$WALLS_ENERGY_EFF)

average["Most Common Walls EE Rating"] <- walls_ee

### What number of properties have no wall insulation ?

no_wall <- Poole %>% 
  filter(str_detect(WALLS_DESCRIPTION, "no insulation")) %>%  
  summarise(total_no_wall_insulation=n(), pct_no_wall_insulation = (n()/(nrow(Poole))) * 100) 

average <- average %>% 
  mutate(no_wall$total_no_wall_insulation, no_wall$pct_no_wall_insulation)


### Possible average savings in lighting

avg_pot_lighting_savings <- Poole %>%
  summarise(avg_pot_lighting_savings=mean(LIGHTING_COST_CURRENT-LIGHTING_COST_POTENTIAL))

average["Avg Potential Lighting Savings (£)"] <- avg_pot_lighting_savings

### Possible average savings in  heating

avg_pot_heating_savings <- Poole %>%
  summarise(avg_pot_heating_savings=mean(HEATING_COST_CURRENT-HEATING_COST_POTENTIAL))

average["Avg Potential Heating Savings (£)"] <- avg_pot_heating_savings

### Possible average savings in Hot water
avg_pot_hot_water_savings <- Poole %>%
  summarise(avg_pot_hot_water_savings=mean(HOT_WATER_COST_CURRENT-HOT_WATER_COST_POTENTIAL))

average["Avg Potential Hot Water Savings (£)"] <- avg_pot_hot_water_savings

write.csv(average, "Poole.csv")


