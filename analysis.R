# installing libraries

install.packages("stringi")  # working with strings
# install.packages("tidyverse", repos = "https://cloud.r-project.org")
install.packages("dbplyr")
# install.packages("tidyverse", dependencies = TRUE)
install.packages("jsonlite")
install.packages("tidyjson")
install.packages("lubridate") # working with dates

# loading packages
library(stringi)
library(stringr)
# library(dplyr)
# library(tidyverse)
library(jsonlite)
library(lubridate)


# importing data
train = read.csv("train.csv")
test = read.csv("test.csv")

dim(train) # 3000 X 23
dim(test) # 4398 X 22

colnames(train)
colnames(test) # revenue is not present

test$revenue = NA

test$data.from = "test"
train$data.from = "train"

# Combining the data
our.data = rbind(train, test)

# Analysising the data
head(our.data)

# Dropping the unwanted datas
features_drop = c(1, 5, 9, 11, 18, 19, 20)
colnames(our.data[features_drop])

our.data = our.data[, -features_drop]

# Analysing belong_to_collection
our.data$belongs_to_collection[1]

our.data$movie_collection = stri_extract(our.data$belongs_to_collection, 
                       regex = "(?<=name\\'\\:\\s{1}\\').+(?=\\'\\,\\s{1}\\'poster_path)")

# add one more feature to check collection is present or not
our.data$collection = ifelse(is.na(our.data$movie_collection), "no collection", "collection")

# Check no of movie in each collection

# Function to find the frequency of value present in a column
showFreq = function(x, varname){
  check.freq = data.frame(count(x, vars = varname))
  check.freq = check.freq[c(order(check.freq$freq, decreasing = TRUE)), ]
  return(check.freq)
}

head(showFreq(x = our.data, "movie_collection"), 10)

# Analysing our.data$genres 
our.data$genres[1]
levels(our.data$genres)

str(our.data$genres)
our.data$genres = as.character(our.data$genres)


# Convert JSON column it to  data frame
getColumn = function(x, colInd, rowNum){

  df <- data.frame()
  
  for (i in 1:rowNum) {
    if (x[i, colInd] != "") {
    
     # # m = gsub(pattern = "[A-Z][\\'][A-Z]", replacement = "",x)
     #  m1 = gsub("\'", "\"", x[i, colInd]) # replace all (') with (")
      
      m = gsub(pattern = "[A-Z][\\'][A-Z]", replacement = "",x[i, colInd])
      m1 = gsub("\'", "\"", m) # replace all (') with (")
      print(i)
      mydf2 = fromJSON(m1)
      df = rbind(df, mydf2) # adding values in our dataframe
     
    }
  }
  return(df)
}


genre.df = getColumn(our.data, which(colnames(our.data) == "genres"), nrow(our.data))

# removing the duplicate value form genre.df
genre.df = genre.df[!duplicated(genre.df), ]

# Write genre.df into csv file
write.csv(genre.df, file = "genre.csv", row.names = FALSE)


# adding columns(Genre type) to our data frame
# and initialize them with 0

gen_col = genre.df$name
our.data[gen_col] = 0

# Adding values to genre columns
# if a movie belongs to the genre insert 1 otherwise 0.

fetch_id = function(x, colInd, rows){
  
  for (r in 1:rows) {
    if (x[r, colInd] != "") {
      
      m1 = gsub("\'", "\"", x[r, colInd]) # replace all (') with (")
      mydf2 = fromJSON(m1)
      
      for(i in 1:nrow(mydf2)){
        # print("Inside mydf for")
        genre_name = genre.df[which(genre.df$id == mydf2$id[i]), 2] # searching genre_name in our genre.df 
        print(genre_name)
        
        if(genre_name %in% colnames(x)){
          # print("Inside if")
          colNo = which(colnames(x) == genre_name)
          # print(colNo)
          # print(r)
          x[r, colNo] = 1
          # x[r, colNo]
        }
      }
      
    }
  }
  return(x)
}

my.data = fetch_id(x = our.data, colInd = 3, nrow(our.data))

# Now, dropping genres column from my.data
my.data = my.data[, -3]

# Also dropping belong_to_collection column from my.data
my.data = my.data[, -1]

# Also dropping imdb_id column from my.data
my.data = my.data[, -2]

# Production Countries
# my.data$production_countries[971]

# Using getColumn function get get the column data frame from production countries column

prod_countries = getColumn(x = my.data, colInd = which(colnames(my.data) == "production_countries"), 
                           rowNum = nrow(my.data))

# Remove duplicate datas
prod_countries = prod_countries[!duplicated(prod_countries), ]

# write it in to csv file
write.csv(prod_countries, file = "prod_countries.csv",row.names = FALSE)

# Function to fetch production countries
getProdCountries = function(x, colInd, rowNum){
  prodCount = c()
  for (i in 1:rowNum) {
    if (x[i, colInd] != "") {
      
      # # m = gsub(pattern = "[A-Z][\\'][A-Z]", replacement = "",x)
      #  m1 = gsub("\'", "\"", x[i, colInd]) # replace all (') with (")
      
      m = gsub(pattern = "[A-Z][\\'][A-Z]", replacement = "",x[i, colInd])
      m1 = gsub("\'", "\"", m) # replace all (') with (")
      # print(i)
      mydf2 = fromJSON(m1)
      country = ""
      for (j in 1:nrow(mydf2)) {
        country = paste(country, mydf2[j, 1], sep = " ")
      }
      prodCount = c(prodCount, country)
    }
    else{
      prodCount = c(prodCount, NA)
    }
  }
  return(prodCount)
}
prod = getProdCountries(my.data, 6, nrow(my.data))

# replace prod_countries in my data with prod
my.data$production_countries = prod

# adding a new feature number of production countries
getNoOfCountries = function(x, colInd){
  noOfCountries = c()
  
  for (i in 1:nrow(x)) {
    if(is.na(x[i, colInd])){
      noOfCountries = c(noOfCountries, 0)
    }
    else{
      country = x[i, colInd]
      country = strsplit(country, split = " ")
      country_mat = matrix(unlist(country), ncol = 1, byrow = T)
      
      noOfCountries = c(noOfCountries, nrow(country_mat)-1)
    }
  }
  
  return(noOfCountries)
}

noOfC = getNoOfCountries(x = my.data, colInd = which(colnames(my.data) == "production_countries"))

my.data$noOfCountries = noOfC
# Original Language
# adding a new feature in my data 'language' having three categorical values
# 'En', 'Hi' and 'others'

addLanguage = function(x, colInd){
  lang = c()
  
  for (r in 1:nrow(x)) {
    if (x[r, colInd] == 'en') {
      lang = c(lang, 'en')
    }
    else if (x[r, colInd] == 'hi') {
      lang = c(lang, 'hi')
    }
    else{
      lang = c(lang, 'others')
    }
  }
  return(lang)
}

movie.lang = addLanguage(x = my.data, which(colnames(my.data) == "original_language"))

# adding movie.lang to our my.data
my.data$language = movie.lang

# Production Companies
getNoOfCompanies = function(x, colInd, rowNum){
  noOfComp = c()
  
  for (i in 1:nrow(x)) {
    print(i)
    prod_comp = x[i, colInd]
    count = str_count(prod_comp, "name")
    noOfComp = c(noOfComp, count)
  }
  
  return(noOfComp)
}

noOfCompanies = getNoOfCompanies(x = my.data, colInd = which(colnames(my.data) == "production_companies"), 
                                 rowNum = nrow(my.data))

# add a new feature noOfCompany to my data
my.data$noOfCompany = noOfCompanies

# ----------------------cast---------------------------------
# count the no of cast
cast_count = str_count(my.data$cast, "name")

# add a feature called cast_count to my data
my.data$cast_count = cast_count

# count the no of female cast
female_cast = str_count(my.data$cast, "gender\': 1")

# add female_cast to my data
my.data$female_cast = female_cast

# count the no of male cast
male_cast = str_count(my.data$cast, "gender\': 2")

# add male_cast to my data
my.data$male_cast = male_cast

# ----------------------crew---------------------------------
# count the no of crew
crew_count = str_count(my.data$crew, "name")

# add a feature called cast_count to my data
my.data$crew_count = crew_count

# count the no of female cast
female_crew = str_count(my.data$crew, "gender\': 1")

# add female_cast to my data
my.data$female_crew = female_crew

# count the no of male cast
male_crew = str_count(my.data$crew, "gender\': 2")

# add male_cast to my data
my.data$male_crew = male_crewD  

# --------------------------- Release Date -------------------------  
r_date = parse_date_time2(my.data$release_date, "mdy", cutoff_2000 = 20)
week_day = wday(r_date, label = T)
mon = month(r_date, label = T)
yr = year(r_date)

# add new features week_day, month and year
my.data$week_day = week_day
my.data$month = mon
my.data$year = yr
