#################
#### PART 1 #####
#################

install.packages("dplyr")
library("dplyr")

# Set working directory and read in any_drinking.csv file
setwd("~/INFO201/a4-data-wrangling-bohran/data")
any.drinking <- read.csv('any_drinking.csv', stringsAsFactors = FALSE)
View(any.drinking)

# Adds new column that takes the difference between male and female drinking amounts
# in 2012
difference.drinking <- any.drinking %>% 
  select(state, location, both_sexes_2012, females_2012, males_2012) %>% 
  mutate(difference.between.sexes = (males_2012 - females_2012))

# Prints the number of locations where females drank more than males
more.female <- filter(difference.drinking, difference.between.sexes < 0) %>% 
  select(state, location, difference.between.sexes)
paste("Females drank more than males in 0 locations.")

# Prints the location where female and male drinking amounts are the closest
males_females_closest <- filter(difference.drinking, difference.between.sexes == min(abs(difference.between.sexes))) %>% 
  select(location, state, difference.between.sexes)
print(males_females_closest)
                          
# Creates new data frame that has state level observations
state.name <- filter(difference.drinking, state == location)

# Prints data frame containing the highest drinking rate for both sexes combined
high.filtered <- filter(state.name, both_sexes_2012 == max(both_sexes_2012))
highest.drinking.rate <- select(high.filtered, state, both_sexes_2012 )
print(highest.drinking.rate)

# Prints data frame containing the lowest drinking rate for both sexes
low.filtered <- filter(state.name, both_sexes_2012 == min(both_sexes_2012))
lowest.drinking.rate <- select(low.filtered, state, both_sexes_2012)
print(lowest.drinking.rate)

# Returns the percentage difference between the combined sexes drinking rates 
# with the highest and lowest drinking rates
high.rate <- select(high.filtered, both_sexes_2012)
low.rate <- select(low.filtered, both_sexes_2012)
difference.rate <- (high.rate - low.rate)
paste("Range in drinking range was", difference.rate,"%")

#################
#### PART 2 #####
#################

# Sets the working directory and reads in the binge drinking csv file
setwd("~/INFO201/a4-data-wrangling-bohran/data")
binge.drinking <- read.csv('binge_drinking.csv', stringsAsFactors = FALSE)
View(binge.drinking)

# Data frame extracted which only contains county level observations
county.name.filtered <- binge.drinking %>%
  filter(state!= "location") %>% filter(state!="National")

# Confirmation that the number of observations is correct
count.initial <- nrow(binge.drinking)
count.filtered <- nrow(county.name.filtered)
print(binge.drinking)
print(count.filtered)

# Add 3 new columns that give information on the change in binge drinking rates
# from 2002 and 2012 for males, females, and both sexes combined
county.name.filtered <- mutate(county.name.filtered, 
                           difference.both = both_sexes_2012 - both_sexes_2002,
                           difference.males = males_2012 - males_2002,
                           difference.females = females_2012 - females_2002
                           )

# Computes the average of binge drinking in 2012 for both sexes
average.both.sexes.2012 <- summarise(county.name.filtered,
                                     mean = mean(both_sexes_2012)
                                     )
print(average.both.sexes.2012)

# Returns the minimum level of binge drinking in each state in 2012 for combined sexes
min.level <- county.name.filtered %>% group_by(state) %>% 
  select(state, location, both_sexes_2012) %>% 
  filter(both_sexes_2012 == min(both_sexes_2012))

# Returns the maximum level of binge drinking in each state in 2012 for combined sexes
max.level <- county.name.filtered %>% group_by(state) %>% 
  select(state, location, both_sexes_2012) %>% 
  filter(both_sexes_2012 == max(both_sexes_2012))

# Returns percent of counties that observed an increase in male binge drinking 
# between 2002 and 2012
percent.increase.males <- county.name.filtered %>% 
  mutate(largest_male_increase = males_2012 - males_2002) %>%
  select(largest_male_increase) %>%
  filter(largest_male_increase > 0)
total.counties <- nrow(county.name.filtered)
total.male.increase <- nrow(percent.increase.males)
total.male.percent <- round(total.male.increase/total.counties * 100, digits = 1)
paste("Male binging increased in", total.male.percent, "% of counties.")

# Returns percent of counties that observed an increase in female binge drinking 
# between 2002 and 2012
percent.increase.females <- county.name.filtered %>% 
  mutate(largest_female_increase = females_2012 - females_2002) %>%
  select(largest_female_increase) %>%
  filter(largest_female_increase > 0)
total.female.increase <- nrow(percent.increase.females)
total.female.percent <- round(total.male.increase/total.counties * 100, digits = 1)
paste("Female binging increased in", total.female.percent, "% of counties.")

# Returns percent of counties that observed an increase in female binge drinking 
# and decrease in male binge drinking between 2002 and 2012
females.increase.males.decrease <- county.name.filtered %>%
  mutate(males_decrease = males_2012 - males_2002) %>% 
  mutate(females_increase = females_2012 - females_2002) %>% 
  filter(males_decrease < 0 , females_increase > 0) 
total.increase.decrease <- nrow(females.increase.males.decrease)
total.percentage <- round(total.increase.decrease/total.counties * 100, digits = 1)
paste("Female binging increased & male binging decreased in",total.percentage, "% of counties")

# Returns the largest median increase in male binge drinking within county level observations
# between 2002 and 2012
largest.male.median <- group_by(county.name.filtered, state) %>% 
  summarise(median = median(males_2012 - males_2002)) %>% 
  filter(median == max(median))

# Returns the largest median increase in female binge drinking within county level observations
# that also observed an increase in female binge drinking and decrease in males
female.large.median <- group_by(females.increase.males.decrease, state) %>% 
  summarise(median.female.drinking.rate = median(females_increase),median.both.drinking.rate = median(difference.both)) %>% 
  filter(median.female.drinking.rate == max(median.female.drinking.rate)) %>% 
  select("state", "median.female.drinking.rate", "median.both.drinking.rate")
print(female.large.median)

#################
#### PART 3 #####
#################

# Replace names of columns within any.drinking and binge.drinking data frames to 
# keep column names unique
colnames(any.drinking) <- paste0('any_',colnames(any.drinking))
colnames(binge.drinking) <- paste0('binge_',colnames(binge.drinking))

# Combines data within any.drinking and binge.drinking data frames to one data frame
# named all.drinking which is joined by location.
all.drinking <- full_join(any.drinking, binge.drinking, by = c("any_location" = "binge_location")) 
all.drinking <- mutate(all.drinking, non.binge.2012 = any_both_sexes_2012 - binge_both_sexes_2012)
all.drinking <- select(all.drinking , -c(binge_state))

# Returns the average rate of drinking that was not binge drinking in 2012 from county levels
county.mean.2012 <- all.drinking %>%
  filter(any_state!= any_location) %>% 
  filter(any_state != "National") %>% 
  summarise(mean = mean(any_both_sexes_2012))
print(county.mean.2012) 

# Returns the name of the state that had the smallest amount of drinking that
# was not binge drinking in 2012, considering state level data
state.smallest.rate <- filter(all.drinking, any_state == any_location) %>% 
  filter(non.binge.2012 == min(non.binge.2012)) %>% 
  select(any_state, any_both_sexes_2012, binge_both_sexes_2012, non.binge.2012)
print(state.smallest.rate)

# Create function that takes in state and year arguments that arranges data in 
# decscending order about drinking rates considering both sexes 
ExportStateYear <- function(input.state, input.year) {
    input.year <- toString(input.year)
    filter(all.drinking, any_state == input.state) %>% 
    select (any_state, any_location, contains(input.year)) %>% 
    arrange_(paste0("any_both_sexes_", input.year)) %>% 
    write.csv(paste0(file = "~/INFO201/a4-data-wrangling-bohran/data/drinking_" , input.state, input.year, ".csv"))
}

# Uses function with state and year data and returns information about drinking rates
ExportStateYear("Washington", 2011)
ExportStateYear("Texas", 2004)

#################
#### PART 4 #####
#################

# Create function to determine the minimum levels of drinking in a state given 
# two population types and years
FindMinimumDrinking <- function(all.drinking, column1, column2) {
  column1.convert <- rlang::sym(column1)
  column2.convert <- rlang::sym(column2)
  print(column1.convert)
  print(column2.convert)
  min.frame <- select(all.drinking, column1.convert == min(!!column1.convert)) %>% 
   select(all.drinking, column2.convert == min(!!column2.convert))
  return(min.frame)                       
}
result <- MinimumDrinking(all.drinking, "any_both_sexes_2002", "any_both_sexes_2012")

