#################
#### PART 1 #####
#################

# Create vector that stores INFO 201 TA's
names <- (c("Adele Miller", "Anirudh Subramanyam", "Zhanna Voloshina", "Tejveer Rai", "Bao Dinh", "Iris Sun"))

# Create vector that stores hypothetical grades in a math course
math.grades <- (c(78, 99, 85, 45, 66, 96))

# Create vector that stores hypothetical grades in a spanish course
spanish.grades <- (c(55, 76, 98, 86, 33, 43))

# Create data frame that combines the TA's, math grades and spanish grades
# and organizes the data into rows and columns
tas <- data.frame(names, math.grades, spanish.grades)

# Generates statement that describes the number of rows and columns the TA data frame contains
# and prints out the names of the columns
tas.data.frame <- paste("The TA data frame has", nrow(tas) , "rows and" , ncol(tas) , "cols:")
column.names <- (paste(colnames(tas), collapse = ","))
print(paste(tas.data.frame, column.names))

# Adds row names in the TA data frame using the names vector created
row.names(tas) <- names

# Create data frame containing my TA's name and grades data
print(tas[1 ,])

# Create new column in data frame that returns the grade difference in the math and spanish courses
tas$grade.diff <- (tas$math.grades - tas$spanish.grades)

# Create new column in data frame that returns true or false if a TA's math grade
# is better than their spanish grade
tas$better.at.math <- tas$grade.diff > 0

# Create variable that counts how many TA's have better math grades than spanish grades
num.better.at.math <- sum(tas$better.at.math)
print(num.better.at.math)

# Writes the TA data frame into a new .csv in data/ directory
write.csv(tas, '~/INFO201/a3-using-data-bohran/data/grade_data.csv', row.names = FALSE)


#################
#### PART 2 #####
#################

# Sets working directory to access 'Data' folder 
setwd("~/INFO201/a3-using-data-bohran/data")

# Reads in life_expectancy.csv file and views its' rows and columns
life.expectancy <- read.csv('life_expectancy.csv', stringsAsFactors = FALSE)
View(life.expectancy)

# Create new column in data frame that returns the change in life expectancy from 1960 to 2013
life.expectancy$change <- (life.expectancy$le_2013 - life.expectancy$le_1960)

# Create variable that returns the number of countries whose life expectancy has not improved
# by 5 years or more between 1960 and 2013.
num.small.gain <- sum(life.expectancy$change < 5)
print(num.small.gain)

# Creates variable to return the name of the country that has the largest gain in life expectancy 
most.improved <- life.expectancy[life.expectancy$change == max(life.expectancy$change), 'country']
print(most.improved)

# Creates function that takes in a country name and returns the change in life expectancy
CountryChange <- function(country) {
  return(life.expectancy$change[life.expectancy$country == country])
}

# Uses the CountryChange function to find the life expectancy change in Haiti
CountryChange('Haiti')

# Returns the country with the lowest life expectancy in 2013 by passing in a region
LowestLifeExpInRegion <- function(region) {
  countries <- life.expectancy[life.expectancy$region == region, ]
  return(life.expectancy$country[life.expectancy$le_2013 == min(countries$le_2013)])  
}

# Uses LowestLifeExpInRegion function to find the lowest life expectancy in 2013
# in the Latin America & Caribbean region
test.country <- LowestLifeExpInRegion("Latin America & Caribbean")
print(test.country)

# Create function that has two country names passed in and returns the name of the country,
# life expectancy in 2013 and the change between 1960 and 2013.
CompareCountries <- function(country1, country2) {
  row.names(life.expectancy) <- life.expectancy$country
  compare <- life.expectancy[c(country1, country2) , c("country" , "le_2013", "change")]
  row.names(compare) <- c("1", "2")
  return(compare)
}

# Uses function to compare the life expectancies between the United States and Cuba
us.vs.cuba <- CompareCountries("United States", "Cuba")


#################
#### PART 3 #####
#################

# Loads the Titanic data set built in R and views the table
Titanic
View(Titanic)

# Converts data set from table to data frame
is.data.frame(Titanic)

# Converts Titanic variable into a data frame. Strings are not treated as factors.
titanic.data.frame <- as.data.frame(Titanic, stringsAsFactors = FALSE)

# Create variable that returns rows containing information about children on Titanic
children <-titanic.data.frame[titanic.data.frame$Age == "Child" ,]

# Create variable that sums the number of children on the Titanic and returns it
children.num <- sum(children$Freq)
print(children.num)

# Returns the maximum number of people that did not survive 
survival <- (titanic.data.frame[titanic.data.frame$Survived == "No" ,])
survival.least <- survival[survival$Freq == max(survival$Freq) ,]
print(survival.least)

# What does the "Class" of this row tell you about how the ship was evacuated?

# The Class row tells us that the Crew was not able to evacuate the ship resulting
# in 670 deaths. They may have priortized getting out passengers, such as children, 
# off the ship first before saving themselves.

# Create function that takes in a Class and returns information about the survival rate 
# amongst the men & women/children that survived the Titanic
SurvivalRate <- function (ticket.class){
  info <- titanic.data.frame[titanic.data.frame$Class == ticket.class, ]
  women.and.children <- info[info$Sex == "Female" | info$Age == "Child", ]
  women.and.children.sum <- sum(women.and.children[women.and.children$Survived == "Yes", "Freq",]) 
  women.and.children.percentage <- round(women.and.children.sum / sum(women.and.children$Freq) * 100, digits = 0)
  men <- info[info$Sex == "Male" ,]
  men.sum <- sum(men[men$Survived == "Yes" , "Freq"])
  men_percentage <- round(men.sum / sum(men$Freq) * 100, digits = 0)
  return(paste("Of" , ticket.class, "class," , women.and.children.percentage, "% of women and children survived and" , men_percentage, "% of men survived."))
}

# Uses the SurvivalRate function to calculate the survival rate of women + children and men by the Class type
SurvivalRate("1st")
SurvivalRate("2nd")
SurvivalRate("3rd")

# What does this data tell you about how the "women and children first" policy was applied across social classes?

# From the data seen in the Titanic data frame, we can see that generally more women and children survived than the men since their 
# lives were prioritized more. However if we look closely, we can see that within the classes, the 1st class women and children had a significantly 
# higher survival rate whereas the 3rd class women and children had a survival rate about half of the 1st class. The 2nd class women and children 
# had about a survival rate that was in the middle of the two other classes.
# This data shows that the higher social class you were, the more your life was "valuable" and prioritized.
