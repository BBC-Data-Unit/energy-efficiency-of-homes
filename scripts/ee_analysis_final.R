setwd('D:/R/R data/R_training')
.libPaths('D:/R/Libraries')
library(tidyverse)
library(janitor)

library(readxl)
library(ggrepel)

### England and Wales
### Using R to help visualise data collected in Energy Efficiency Master
### Reading in the master spreadsheet and cleaning category names

la_master <- read_xlsx("Energy Efficiency by LA - MASTER.xlsx")

la_master <- clean_names(la_master)

names(la_master)

###creating basic graph for CO2


ggplot(la_master, aes(x = local_authority, y = average_co2_emissions, size = average_co2_emissions)) +
  geom_point(alpha = 0.3, colour = "blue", label = "local_authority") +
  ggtitle('Avg CO2 emissions by local authority') +
  geom_text_repel(aes(label=if_else(average_co2_emissions>=6.1|average_co2_emissions<=3.3,as.character(local_authority),""), show.legend = F)) +
  geom_hline(yintercept = 4.2, size = 1, colour="red") +
  labs(y="CO2 emissions", caption = "Data from the BBC SDU") +
  scale_y_continuous(labels=scales::comma)+
  theme(panel.background = element_blank())


### filtering for ten highest areas of Co2

la_co2 <- la_master %>% 
  filter(average_co2_emissions >= 5) %>% 
  head(10) %>% 
  arrange(desc(average_co2_emissions))

ggplot(la_co2, aes(x=reorder(local_authority, average_co2_emissions), y=average_co2_emissions, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "Local Authorities with highest CO2 emissions", x = "", y = "CO2 (mt CO2e)", fill = "Avg CO2") +
  coord_flip()

### ten best (lowest) areas for average CO2 emissions

best_la_co2 <- la_master %>% 
  filter(average_co2_emissions <= 4) %>% 
  tail(10) %>% 
  arrange(desc(average_co2_emissions))

ggplot(best_la_co2, aes(x=reorder(local_authority, -average_co2_emissions), y=average_co2_emissions, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "Local Authorities with lowest CO2 emissions", x = "", y = "CO2 (mt CO2e)", fill = "Avg CO2") +
  coord_flip()

### Areas with the highest/lowest potential improvements in C02 emissions and the difference between currewnt and potential output

potential_co2 <- la_master %>% 
  filter(average_potential_c02_emissions>= 2) %>% 
  head(10) %>% 
  arrange(desc(average_potential_c02_emissions))

ggplot(potential_co2, aes(x=reorder(local_authority, average_potential_c02_emissions), y=average_potential_c02_emissions, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "LA's with biggest potential improvements in CO2 after upgrades ", x = "", y = "CO2 (mt CO2e)", fill = "Avg CO2") +
  coord_flip()

reduction_co2 <- la_master %>% 
  rowwise() %>% 
  mutate(reduction = sum(average_co2_emissions-average_potential_c02_emissions))

reduction_filter <- reduction_co2 %>% 
  filter(reduction <= 4) %>% 
  head(10) %>% 
  arrange(desc(reduction))

ggplot(reduction_filter, aes(x=reorder(local_authority, reduction), y=reduction, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "Local Authorities with highest potential CO2 reduction", x = "", y = "CO2 (mt CO2e)", fill = "Avg CO2") +
  coord_flip()

potential_co2 <- la_master %>% 
  filter(average_potential_c02_emissions>= 2) %>% 
  tail(10) %>% 
  arrange(desc(average_potential_c02_emissions))

ggplot(potential_co2, aes(x=reorder(local_authority, average_potential_c02_emissions), y=average_potential_c02_emissions, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "LA's with lowest potential improvements in CO2 after upgrades ", x = "", y = "CO2 (mt CO2e)", fill = "Avg CO2") +
  coord_flip()


### What are the highest/lowest areas in terms of energy efficiency
### creating a subset without extra column and rows of NA
la_crop = subset(la_master, select = -c(x31))

la_crop2 <- na.omit(la_crop) 

### looking at energy efficiency measure
high_ee <- la_crop2 %>% 
  summarise(high_ee = mean(average_energy_efficiency))

min_ee <- min(la_crop2[,12])
max_ee <- max(la_crop2[,12])

la_min <- la_crop2 %>% 
  filter(average_energy_efficiency == min_ee)

la_max <- la_crop2 %>% 
  filter(average_energy_efficiency == max_ee)

### what is the total number of households without gas central heating?

#without
cen_heat <- colSums(la_master[,c(5)], na.rm = T)
#with
cen_heat_2 <- colSums(la_master[,c(4)], na.rm=T)

### what would be the combined cost of replacing the gas central heating with the most environmentally friendly alternative?
### cost based on Govt base case of £6,725 for a Hybrid Heat Pump system

cen_heat_cost <- cen_heat+cen_heat_2*6725

### what are the total number of properties without wall and roof insulation?
no_insulation <- colSums(la_crop2[,c(21,25)], na.rm = T)

no_insulation


### Which local authorities have the highest cost of improving properties.

improve_cost <- la_crop2 %>% 
  mutate(total_improvement_cost = rowSums(select(.,starts_with("est_"))))

high_cost <- improve_cost %>% 
  arrange(desc(total_improvement_cost)) %>% 
  head(20)

ggplot(high_cost, aes(x = reorder(local_authority, total_improvement_cost), y = total_improvement_cost)) +
  geom_col() + 
  labs(x = "Local Authority", y = "Cost in £ (m)", title = "LA's with highest cost of improvements") +
  coord_flip() +
  theme_classic()

low_cost <- improve_cost %>% 
  arrange(desc(total_improvement_cost)) %>% 
  tail(20)

ggplot(low_cost, aes(x = reorder(local_authority, -total_improvement_cost), y = total_improvement_cost)) +
  geom_col() + 
  labs(x = "Local Authority", y = "Cost in £ (m)", title = "LA's with lowest cost of improvements") +
  coord_flip() +
  theme_classic()


### what is the national figure?
### culmalative national total
eng_wales_total <- colSums(improve_cost[31], na.rm = T)

### national cost per measure

eng_wales_dg_total <- colSums(improve_cost[20], na.rm = T)

eng_wales_roof_total <- colSums(improve_cost[23], na.rm = T)

eng_wales_wall_total <- colSums(improve_cost[27], na.rm = T)

nat_total <- data.frame(eng_wales_total,eng_wales_dg_total,eng_wales_roof_total,eng_wales_wall_total)


### Which area has most grade G properties?

grade_g <- la_crop2 %>% 
  arrange(desc(number_of_properties_in_grade_g)) %>% 
  head(20)


ggplot(grade_g, aes(x=reorder(local_authority, number_of_properties_in_grade_g), y=number_of_properties_in_grade_g, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "Local Authorities with highest number of Grade G", x = "", y = "Grade G properties", fill = "Avg CO2") +
  coord_flip()

# which area had fewest grade g properties?

grade_g_few <- la_crop2 %>% 
  arrange(desc(number_of_properties_in_grade_g)) %>% 
  tail(20)

ggplot(grade_g_few, aes(x=reorder(local_authority, number_of_properties_in_grade_g), y=number_of_properties_in_grade_g, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "Local Authorities with lowest number of Grade G", x = "", y = "Grade G properties", fill = "Avg CO2") +
  coord_flip()

### which area has the highest average potential saving if all improvement measures are implemented?

total_savings <- la_crop2 %>% 
  mutate(total_average_savings = rowSums(select(.,starts_with("avg_potential_savings"))))

most_savings <- total_savings %>% 
  arrange(desc(total_average_savings)) %>% 
  head(20)

ggplot(most_savings, aes(x = reorder(local_authority, total_average_savings), y = total_average_savings)) +
  geom_col() + 
  labs(x = "Local Authority", y = "Savings in £", title = "LA's with highest potential savings") +
  coord_flip() +
  theme_classic()

low_savings <- total_savings %>% 
  arrange(desc(total_average_savings)) %>% 
  tail(20)

ggplot(low_savings, aes(x = reorder(local_authority, -total_average_savings), y = total_average_savings)) +
  geom_col() + 
  labs(x = "Local Authority", y = "Cost in £ ", title = "LA's with lowest potential savings") +
  coord_flip() +
  theme_classic()




### national saving per measure

ew_light_savings <- colSums(total_savings[28], na.rm = T)/346

ew_heat_savings <- colSums(total_savings[29], na.rm = T)/346

ew_hw_savings <- colSums(total_savings[30], na.rm = T)/346

ew_savings <- (ew_light_savings+ew_heat_savings+ew_hw_savings)


nat_sav_total <- data.frame(ew_savings,ew_light_savings,ew_heat_savings,ew_hw_savings)
colnames(nat_sav_total)[colnames(nat_sav_total)=="avg_potential_savings_lighting"] <- "average_savings"


#### And now for Scotland

scot_la_master <- read_xlsx("Energy Efficiency by LA - MASTER.xlsx", sheet = "Energy Efficiency Scotland")

scot_la_master <- clean_names(scot_la_master)

names(scot_la_master)
 
###creating basic graph for CO2

ggplot(scot_la_master,(aes(x = local_authority, y = average_c02, size = average_c02))) +
         geom_point(alpha = 0.3, colour = "blue", label = "local_authority")  +
         ggtitle('Avg CO2 emissions by local authority')  +
         geom_text_repel(aes(label=if_else(average_c02>=6.1|average_c02<=4,as.character(local_authority),""), show.legend = F)) +
         geom_hline(yintercept = 5.04, size = 1, colour="red")  +
         labs(y="CO2 emissions", caption = "Data from the BBC SDU")  +
         scale_y_continuous(labels=scales::comma) +
         theme(panel.background = element_blank()) 


### filtering for ten highest areas of Co2

scot_la_co2 <- scot_la_master %>% 
  filter(average_c02 >= 5) %>% 
  head(10) %>% 
  arrange(desc(average_c02))

ggplot(scot_la_co2, aes(x = reorder(local_authority, average_c02), y = average_c02)) +
  geom_point() + 
  coord_flip()

ggplot(scot_la_co2, aes(x = reorder(local_authority, -average_c02), y = average_c02)) +
  geom_point() + 
  coord_flip()

#creating a coloured version of La's with highest CO2 emissions
ggplot(scot_la_co2, aes(x=reorder(local_authority, average_c02), y=average_c02, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "Local Authorities with highest CO2 emissions", x = "", y = "CO2 (mt CO2e)", fill = "Avg CO2") +
  coord_flip()

### ten best (lowest) areas for average CO2 emissions

best_scot_co2 <- scot_la_master %>% 
  filter(average_c02 <= 5) %>% 
  tail(10) %>% 
  arrange(desc(average_c02))

ggplot(best_scot_co2, aes(x=reorder(local_authority, -average_c02), y=average_c02, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "Local Authorities with lowest CO2 emissions", x = "", y = "CO2 (mt CO2e)", fill = "Avg CO2") +
  coord_flip()

### Areas with the highest/lowest potential improvements in C02 emissions and the difference between currewnt and potential output

scot_pot_co2 <- scot_la_master %>% 
  filter(average_co2_potential >= 2) %>% 
  head(10) %>% 
  arrange(desc(average_co2_potential))

ggplot(scot_pot_co2, aes(x=reorder(local_authority, average_co2_potential), y=average_co2_potential, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "LA's with biggest potential improvements in CO2 after upgrades ", x = "", y = "CO2 (mt CO2e)", fill = "Avg CO2") +
  coord_flip()

scot_red_co2 <- scot_la_master %>% 
  rowwise() %>% 
  mutate(reduction = sum(average_c02-average_co2_potential))

scot_red_filter <- scot_red_co2 %>% 
  filter(reduction <= 4) %>% 
  head(10) %>% 
  arrange(desc(reduction))

ggplot(scot_red_filter, aes(x=reorder(local_authority, reduction), y=reduction, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "Local Authorities with highest potential CO2 reduction", x = "", y = "CO2 (mt CO2e)", fill = "Avg CO2") +
  coord_flip()

scot_pot_filt <- scot_la_master %>% 
  filter(average_co2_potential>= 2) %>% 
  tail(10) %>% 
  arrange(desc(average_co2_potential))

ggplot(scot_pot_filt, aes(x=reorder(local_authority, average_co2_potential), y=average_co2_potential, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "LA's with lowest potential improvements in CO2 after upgrades ", x = "", y = "CO2 (mt CO2e)", fill = "Avg CO2") +
  coord_flip()


### what is the total number of households without gas central heating?

#without
scot_cen_heat <- colSums(scot_la_master[,c(5)], na.rm = T)

#with
scot_cen_heat_2 <- colSums(scot_la_master[,c(4)], na.rm=T)

### what would be the combined cost of replacing the gas central heating with the most environmentally friendly alternative?
### cost based on Govt base case of £6,725 for a Hybrid Heat Pump system

scot_cen_heat_cost <- scot_cen_heat+scot_cen_heat_2*6725 


### what are the total number of properties without wall and roof insulation?
no_insulation_scot <- colSums(scot_la_master[,c(22,25)], na.rm = T)

no_insulation_scot

colnames(scot_la_master)[colnames(scot_la_master) =="e"] <- "est_cost_of_double_glazing_remaining_properties"
### Which local authorities have the highest cost of improving properties.

scot_improve_cost <- scot_la_master %>% 
  mutate(total_improvement_cost = rowSums(select(.,starts_with("est_"))))

scot_high_cost <- scot_improve_cost %>% 
  arrange(desc(total_improvement_cost)) %>% 
  head(20)

ggplot(scot_high_cost, aes(x = reorder(local_authority, total_improvement_cost), y = total_improvement_cost)) +
  geom_col() + 
  labs(x = "Local Authority", y = "Cost in £ (m)", title = "LA's with highest cost of improvements") +
  coord_flip() +
  theme_classic()

scot_low_cost <- scot_improve_cost %>% 
  arrange(desc(total_improvement_cost)) %>% 
  tail(20)

ggplot(scot_low_cost, aes(x = reorder(local_authority, -total_improvement_cost), y = total_improvement_cost)) +
  geom_col() + 
  labs(x = "Local Authority", y = "Cost in £ (m)", title = "LA's with lowest cost of improvements") +
  coord_flip() +
  theme_classic()


### what is the national figure?

scot_total <- colSums(scot_improve_cost[31], na.rm = T)


### national cost per measure

scot_dg_total <- colSums(scot_improve_cost[21], na.rm = T)

scot_roof_total <- colSums(scot_improve_cost[24], na.rm = T)

scot_wall_total <- colSums(scot_improve_cost[27], na.rm = T)

scot_nat_total <- data.frame(scot_total,scot_dg_total,scot_roof_total,scot_wall_total)




### Which area has most grade G properties?

scot_grade_g <- scot_la_master %>% 
  arrange(desc(number_of_properties_in_grade_g)) %>% 
  head(20)


ggplot(scot_grade_g, aes(x=reorder(local_authority, number_of_properties_in_grade_g), y=number_of_properties_in_grade_g, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "Local Authorities with highest number of Grade G", x = "", y = "Grade G properties", fill = "Avg CO2") +
  coord_flip()

# which area had fewest grade g properties?

scot_grade_g_few <- scot_la_master %>% 
  arrange(desc(number_of_properties_in_grade_g)) %>% 
  tail(20)

ggplot(scot_grade_g_few, aes(x=reorder(local_authority, number_of_properties_in_grade_g), y=number_of_properties_in_grade_g, fill= ..y..)) +
  geom_bar(stat = "identity") +
  labs(title = "Local Authorities with lowest number of Grade G", x = "", y = "Grade G properties", fill = "Avg CO2") +
  coord_flip()

### which area has the highest average potential saving if all improvement measures are implemented?

scot_total_savings <- scot_la_master %>% 
  mutate(total_savings = rowSums(select(.,starts_with("average_potential_"))))


scot_most_savings <- scot_total_savings %>% 
  arrange(desc(total_savings)) %>% 
  head(20)

ggplot(scot_most_savings, aes(x = reorder(local_authority, total_savings), y = total_savings)) +
  geom_col() + 
  labs(x = "Local Authority", y = "Savings in £", title = "LA's with highest potential savings") +
  coord_flip() +
  theme_classic()

scot_low_savings <- scot_total_savings %>% 
  arrange(desc(total_savings)) %>% 
  tail(20)

ggplot(scot_low_savings, aes(x = reorder(local_authority, -total_savings), y = total_savings)) +
  geom_col() + 
  labs(x = "Local Authority", y = "Cost in £ ", title = "LA's with lowest potential savings") +
  coord_flip() +
  theme_classic()


### national saving per measure

scot_light_savings <- colSums(scot_la_master[28], na.rm = T)/33

scot_heat_savings <- colSums(scot_la_master[29], na.rm = T)/33

scot_hw_savings <- colSums(scot_la_master[30], na.rm = T)/33

scot_savings <- (scot_light_savings+scot_heat_savings+scot_hw_savings)


scot_sav_total <- data.frame(scot_light_savings,scot_heat_savings,scot_hw_savings,scot_savings)
colnames(nat_sav_total)[colnames(nat_sav_total) =="average_potential_lighting_savings_three_year"] <- "average_savings"

