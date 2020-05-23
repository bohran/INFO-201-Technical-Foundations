#install.packages("shiny")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("maps")
#install.packages("plotly")
#install.packages("sp")
#install.packages("maptools")
#install.packages('rsconnect')

library("shiny")
library("tidyr")
library("dplyr")
library("ggplot2")
library("maps")
library("plotly")
library("sp")
library("maptools")

source("shinySetup.R")
source("spatial_utils.R")

# Reads in CO2 emission .csv data files
data <- read.csv("data/WDI_emissions_Data.csv", stringsAsFactors = FALSE)
source.data <- read.csv("data/WDI_emissions_Definition and Source.csv", stringsAsFactors = FALSE)

# Rename columns in dataframe (mainly the "YEAR" columns)
colnames(data) <- c("Country.Code", "Series.Code", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", 
                    "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "Most Recent")

# Filters data for Series.Code data relevent to the data set analysis
filtered.data <- select(data, Country.Code, Series.Code, c(1998:2014, "Most Recent")) %>% 
 na.omit(filtered.data) %>% 
  filter(Series.Code == "EN.ATM.CO2E.KT" | Series.Code == "EN.ATM.CO2E.PC")

# Runs ShinyApp
shinyApp(ui = my.ui, server = my.server)



  
