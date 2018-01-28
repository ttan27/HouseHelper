# ===================================================================
# Title: Global
# Description:
#   This script web scrapes and creates csv files.
# Input(s): URLs
# Output(s): CSV file
# Author: Timothy Tan
# Date: 12-27-2017
# ===================================================================

#web scraping
library(XML)
library(xml2)
library(rvest)
library(stringr)
library(magrittr)

#csv reading
library(readr)

#new recipe maker
saveNewRecipe <- function(ingredients = data.frame(Ingredient = character(0), 
                                                   Amount = numeric(0), Unit = character(0),
                                                   Serves = numeric(0)), title, serves){
  recipeList <- read_csv("recipes.csv", col_names = TRUE)
  csvName <- str_to_lower(title)
  csvName <- str_replace_all(csvName, " ", "")
  recipeList[nrow(recipeList) + 1,] <- c(title, csvName, serves)
  write_csv(recipeList, path = "recipes.csv", col_names = TRUE)
  write_csv(ingredients, path = paste('recipes/', csvName, '.csv', sep = ''), col_names = TRUE)
}

inventory <- read_csv("inventory.csv")

#get recipe
getRecipeList <- function() {
  recipeList <- read_csv("recipes.csv")
  return(recipeList$name)
}

getRecipe <- function(title) {
  csvName <- str_to_lower(title)
  csvName <- str_replace_all(csvName, " ", "")
  recipe <- read_csv(paste('recipes/', csvName, '.csv', sep = ''), col_names = TRUE)
  return(recipe)
}

getServesCount <- function(title) {
  recipeList <- read_csv("recipes.csv")
  for(i in 1:nrow(recipeList)){
    if (recipeList$name[i] == title) {
      return(recipeList$serves[i])
    }
  }
  return(1)
}

createShoppingList <- function(items = data.frame(Ingredient = character(0), 
                                                  Amount = numeric(0), Unit = character(0),
                                                  stringsAsFactors = FALSE)) {
  myInventory <- getInventory()
  shoppingList <- data.frame(Ingredient = character(0), Amount = numeric(0), Unit = character(0),
                             Location = character(0), stringsAsFactors = FALSE)
  for(i in 1:nrow(items) ){
    for(j in 1:nrow(myInventory)) {
      if ((str_to_lower(items$Ingredient[i])) == (str_to_lower(myInventory$name[j]))) {
        myInventory$stock[j] <- myInventory$stock[j] - items$Amount[j]
      }
      # if ((str_to_lower(items$Ingredient[i])) == 'potato') {
      #   myInventory$stock[j] <- myInventory$stock[j] - items$Amount[j]
      # }
    }
  }
  myInventory <- na.omit(myInventory)
  myInventory$stock <- as.numeric(myInventory$stock)
  for(i in 1:nrow(myInventory)) {
    if(myInventory$stock[i] == 0) {
      shoppingList[nrow(shoppingList) + 1,] <- c(myInventory$name[i], myInventory$need[i], myInventory$unit[i],
                                                 myInventory$location[i])
    }
    if(myInventory$stock[i] < 0) {
      shoppingList[nrow(shoppingList) + 1,] <- c(myInventory$name[i], abs(myInventory$stock[i]), myInventory$unit[i],
                                                 myInventory$location[i])
    }
  }
  print(myInventory[myInventory$name == "potatoes",])
  shoppingList$Amount <- as.numeric(shoppingList$Amount)
  saveInventory(myInventory)
  return(shoppingList)
}

getInventory <- function() {
  return (read_csv("inventory.csv"))
}

saveInventory <- function(inv = data.frame(name = character(0), stock = numeric(0),
                                           need = numeric(0), unit = character(0), 
                                           location = character(0), stringsAsFactors = FALSE)){
  write_csv(inv, "inventory.csv")
}

itemTable <- data.frame(Ingredient = character(0), 
                        Amount = numeric(0), Unit = character(0), stringsAsFactors = FALSE)
itemTable[1,] <- c("potato", 2, "lb")
itemTable[2,] <- c("bacon", 2, "lb")
itemTable[3,] <- c("ground beef", 3, "lb")
itemTable$Amount <- as.numeric(itemTable$Amount)

newTable <- createShoppingList(itemTable)
test <- getInventory()

