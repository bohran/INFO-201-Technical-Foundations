# Reksha Rathnam 
# Anant Rajeev 
# Nicole Bohra
# 2/12/18 
# INFO 201 HW 6 


#install.packages("ggplot2")

library("knitr")
library("dplyr")
library("ggplot2")
library("kableExtra")

info <- read.csv('data/intro-survey.csv')

# Summary Statistics Description 
mean.program.exp <- round(summarise(info, mean(programming_exp)), digits = 2) 
from.washington <- count(filter(info, washington_born == "Yes"))
windows.count <- count(filter(info, os == "Windows"))
mac.count <- count(filter(info, os == "Mac OS X"))
coffee.max <- summarise(info, max(coffee_cups))
seahawks.superfans <- count(filter(info, seahawks_fan == "Yes"))
percent.info.interest <- round((count(filter(info, info_interest == "Yes"))) / (count(info)), digits = 1) * 100

#### Response Statistics Table ####
program.exp <- select(info, programming_exp, cli_exp, vcs_exp, md_exp, r_exp, web_exp)

rank.num <- function(rank){
  rank.vector <- c((nrow(filter(program.exp, programming_exp == rank))),
                   (nrow(filter(program.exp, cli_exp == rank))),
                   (nrow(filter(program.exp, vcs_exp == rank))),
                   (nrow(filter(program.exp, md_exp == rank))),
                   (nrow(filter(program.exp, r_exp == rank))),
                   (nrow(filter(program.exp, web_exp == rank))))
  return (rank.vector)
}

rank.one <- rank.num(1)
rank.two <- rank.num(2)
rank.three <- rank.num(3)
rank.four <- rank.num(4)
rank.five <- rank.num(5)

exp.data <- data.frame(rank.one, rank.two, rank.three, rank.four, rank.five)
names <- c("Program", "Command_Line", "Version_Control", "Markdown", "R", "Web")
row.names(exp.data) <- names

# First Visualization
first.plot <- ggplot(data = info) +
  geom_smooth(mapping = aes(x = programming_exp, y = cli_exp, color = "red")) +
  geom_smooth(mapping = aes(x = programming_exp, y = r_exp, color = "black")) + 
  geom_smooth(mapping = aes(x = programming_exp, y = web_exp, color = "blue")) + 
  xlab("Programming Experience") +
  ylab("Experience") +
  ggtitle("Overall Programming Experience vs Specific Experience") +
  scale_color_manual(labels = c("Web","R","Command Line"), values = c("red","black","blue"))
print(first.plot)

# Second Visualization
info <- filter(info, coffee_cups < 80000)
second.plot <- ggplot(data = info) + 
  geom_point(mapping = aes(x = seahawks_fan, y = coffee_cups, color = washington_born)) +
  xlab("Seahawks Fan") +
  ylab("Coffee Cups") +
  ggtitle("Coffee Cups vs. Seahawks Fans vs. Born in Washington")
print(second.plot)

