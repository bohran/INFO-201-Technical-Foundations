#################
#### PART 1 #####
#################

install.packages("stringr")
library("stringr")

# Create variable my.age to store my age
my.age <- 20

# Create variable my.name to store my name
my.name <- "Nicole Bohra"

# Create function to return an introduction by passing name and age
MakeIntroduction <- function(name, age) {
  intro <- paste("Hello my name is", name, "and my age is", age)
return(intro)
}
MakeIntroduction(my.name, my.age)

# Create introduction statement using my.name and my.age arguments
my.intro <- MakeIntroduction(my.name, my.age)
print(my.intro)

# Revising introduction statement by replacing "Hello" with "Hey" 
casual.intro <- sub("Hello my name is", "Hey I'm", my.intro)
print(casual.intro)

# Capitalizes the beginning of every word in introduction statement
capital.intro <- str_to_title(my.intro)
print(capital.intro)

# Counts the number of times the letter "e" (lower case) shows up in introduction statement
intro.e.count <- str_count(my.intro, "e")
print(intro.e.count) 


#################
#### PART 2 #####
#################

# Create vector storing the titles of six books
books <- c("Eragon", "Kite Runner", "Hamlet", "Winnie the Pooh", "1984", "Animal Farm")
print(books)

# Prints out top three books in vector 
indices <- c(1, 2, 3)
top.three.books <- books[indices]
print(top.three.books)

# Adds "It's a great read" statement after every element in 'books' vector
comment <- c("It's a great read!")
book.reviews <- paste(books, comment) 
print(book.reviews)
  
# Create function to remove a title from the vector by the passing in 
# the vector name and the index it is positioned at
RemoveBook <- function(books, index) {
  remove.title <- books[-index]
  return(remove.title) 
}

# Removes the title of the book from the fourth index in the vector
books.without.four <- RemoveBook(books, 4)
print (books.without.four)

# Returns the books that have more than 15 characters in the title
indices <- nchar(books) > 15
long.title <- books[indices]
print(long.title)

#################
#### PART 3 #####
#################

# Creates "numbers" variable that contains the numbers 1 to 201
numbers <- 1:201

# Computes the square of each number in the vector and returns the result
squared.numbers <- (numbers ^ 2)
print(squared.numbers)

# Computes and returns the average of all the squared numbers 
squared.mean <- mean(squared.numbers)
print(squared.mean)

# Returns perfect squares by checking boolean values and seeing if 
# the square root returns a whole number
squares <- sqrt(numbers)
boolean.vector <- round(squares) == squares
numbers[boolean.vector]

#################
#### PART 4 #####
#################

# Create variable to represent the first day of Spring Break using asDate() function
spring.break <- as.Date(c("2018-03-17"))

# Create variable to represent today's date using Sys.Date() function
today <- Sys.Date()
print (today)

# Create variable to return the number of days until Spring Break from today's break 
days.to.break <- (spring.break - today)
print(days.to.break)

# Create function that uses name, age and birthday Date object to return a statement indicating 
# the number of days till an individual's birthday from the current date 
MakeBirthdayIntro <- function(name, age, date) {
  birth.day <- as.Date(c(date))
  daysBirthday <- (birth.day - today)
  if(daysBirthday < 0) {
    daysBirthday <- daysBirthday + 365
  }
  birthday <- paste(my.intro, ". In", daysBirthday,  "days I'll be", age + 1)
  return(birthday)
}
MakeBirthdayIntro(my.name, my.age, "2018-01-16")

#Create variable to print out birthday statement by passing in name, age and birthday arguments
my.bday.intro <- MakeBirthdayIntro(my.name, my.age, "2018-06-04")
print(my.bday.intro)
