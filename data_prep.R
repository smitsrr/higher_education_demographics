# Use this file to transform downloaded NCES data into tidy data ready for 
# visualization!


library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

## I want to re-do the data pull and exclude Gender from the inclusion. 
# Think about whether we want to do any aggregation and whether we'll need
# to continue to add/subtract the Totals to be able to do that to have a consistent
# denominator. 

#I could have downloaded all of these in one, but I didn't. 
inst<- read.csv("raw_data/Data_2-22-2020---191.csv", stringsAsFactors = F)
lookups<- read.csv("raw_data/ValueLabels_2-22-2020---191.csv", stringsAsFactors = F)
dems_1<- read.csv("raw_data/ipeds_2-22-2020.csv", stringsAsFactors = F)
level_lookup<-read.csv("raw_data/inst_level_labels.csv", stringsAsFactors = F) %>%
  select(-VariableName)%>%
  rename("Level of Institution" = ValueLabel)
inst_level<-read.csv("raw_data/inst_level.csv", stringsAsFactors = F) %>%
  select(-Institution.Name, -X) %>%
  left_join(level_lookup, by = c("Level.of.institution..HD2018."="Value")) %>%
  select(-Level.of.institution..HD2018.)

#Value labels
control<- filter(lookups, VariableName == "Control of institution (HD2018)") %>%
  select(-VariableName) %>%
  mutate(Value = as.integer(Value)) %>%
  rename("Public or Private" = "ValueLabel")
institutional_control<-filter(lookups, VariableName =="Institutional control or affiliation (IC2018)") %>%
  select(-VariableName) %>%
  mutate(Value = as.integer(Value)) %>%
  rename("Institution Type" = "ValueLabel")

## INSTITUTION DATA
inst_clean <- inst %>%
  #decided I only want to use the most recent tuition since it's just a reference and I might end up bucketing it anyway. 
  select(-X, -Published.in.district.tuition.and.fees.2015.16..IC2015_AY.,
         -Published.in.district.tuition.and.fees.2016.17..IC2016_AY.,
         -Published.in.district.tuition.and.fees.2017.18..IC2017_AY.,
         -Religious.affiliation..IC2018.) %>%
  left_join(control, by = c("Control.of.institution..HD2018."="Value")) %>%
  left_join(institutional_control, by = c("Institutional.control.or.affiliation..IC2018."="Value")) %>%
  rename("Tuition"= "Published.in.district.tuition.and.fees.2018.19..IC2018_AY.",
         "Address" = "Street.address.or.post.office.box..HD2018.", 
         "City" = "City.location.of.institution..HD2018.",
         "Zip" = "ZIP.code..HD2018.",
         "State" = "State.abbreviation..HD2018.") %>%
  #we have the labels, so dropping the codes
  select(-Institutional.control.or.affiliation..IC2018.,
         -Control.of.institution..HD2018.) %>%
  left_join(inst_level)
#decided I don't care about religious affiliation since Instiutional Contraol
# already lists if they have a religious affiliation. 


## DEMOGRAPHIC DATA
#Curse them for labeling them 'men' and 'women' instead of 'male' and 'female'!

dems_2<- dems_1 %>%
  select(-X, -Institution.Name) %>%
  pivot_longer(-UnitID, names_to="variable", values_to="count") %>%
  #pull out faculty/student, year, and gender
  mutate(students_faculty = ifelse(grepl("All.instructional",variable), "Faculty", "Students"),
         Year = str_extract(variable, "20[0-9]{2}"),
         Gender = ifelse(is.na(str_extract(variable, "women")), "Male", "Female"),
         Ethnicity = case_when(
           grepl("White", variable) ~ "White", 
           grepl("Native.Hawaiian", variable) ~ "Native Hawaiian/Pacific Islander", 
           grepl("Two.or.", variable) ~ "Two or more", 
           grepl("Hispanic", variable) ~ "Hispanic", 
           grepl("Black.or", variable) ~ "Black or African American", 
           grepl("Asian", variable) ~ "Asian", 
           grepl("American.Indian.", variable) ~ "American Indian or Alaska Native", 
           grepl("Race.ethnicity", variable) ~ "Unknown",
           grepl("Grand.total", variable) ~ "Total",
           grepl("Nonresident.a",variable)~"Nonresident Alien"
         )) %>%
  arrange(UnitID, Year, students_faculty, Gender, Ethnicity) %>%
  select(-variable) %>%
  #Confirmed taht the Total is in fact the sum of the others. Therefore I'm going to drop
  # these rows and make my own. 
  filter(Ethnicity != "Total") %>%
  left_join(inst_clean) %>%
  # get each ethnicity count
  group_by(UnitID, Institution.Name, Address, City, State, Zip,
          Tuition, `Public or Private`, `Institution Type`, 
          `Level of Institution`, Year, students_faculty, Ethnicity) %>%
  summarize(count = sum(count)) %>%
  select(UnitID, Institution.Name, Address, City, State, Zip, Tuition, 
         `Public or Private`, `Institution Type`, `Level of Institution`, 
         Year, students_faculty, Ethnicity, count) %>%
  # group by and add a column at the year, student-faculty level
  group_by(UnitID, Year, students_faculty) %>%
  mutate(eth_total = sum(count))



 # There are some schools with literally no Count data??
df <- dems_2 %>%
  group_by(UnitID, Institution.Name, `Public or Private`, Tuition, Year, students_faculty) %>%
  summarize(tot_pop = sum(count, na.rm = T)) %>%
  # 3315 schools have 0 students or 0 faculty. hmmm. there are only 6857 schools in the entire dataset. 
  filter(tot_pop == 0 )
  # No schools have NA values
  filter(is.na(tot_pop))
  
  
  #I'm just going to make the call that if a school has 0 students OR faculty
  # in 2018, we aren't going to include them. This is weird!
#Figure out which schools to exclude
excluded_schools<- dems_2 %>%
  group_by(UnitID, students_faculty, Year) %>%
  summarise(tot_pop = sum(count, na.rm = T)) %>%
  filter(Year == 2018 &
           tot_pop == 0) %>%
  group_by(UnitID) %>%
  tally()
  # 3164 schools get excluded. that's crazy!

dems_3 <- anti_join(dems_2, excluded_schools)

# need to calculate the percent of each minority. those earlier analyses excluded non-resident
# alien and unknown, which is fine, but i'm not going to exxclude them from the denominator. 

# BigQuery doesn't like 'NA' as a character for NA values, so have to encode as empty string
write.csv(dems_3, "tidy_demographics.csv", row.names = F, na = "")


## Just some exploration. 
## Ggplot can't really handle rending this much data very well. takes a few minutes
#    to render a plot of just one year's worth of data. 
recent<- filter(dems_3, Year == 2018 &
                  Ethnicity != 'Unknown' &
                  Ethnicity != 'Nonresident Alien') 

ggplot(recent, aes(x=Ethnicity, y=count/eth_total))+
  geom_jitter() +
  facet_wrap(~students_faculty) + 
  coord_flip()

# Check numbers:
df<- dems_3 %>%
  group_by(students_faculty, Ethnicity) %>%
  summarize(tot = sum(count, na.rm = T)) 
