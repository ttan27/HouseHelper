# HouseHelper

In Progress

Current Functionality:
* Can create new recipe and save it to recipe folder
* Select and scale recipe from list of existing recipes
* Add desired items and selected recipe to shopping list
* Create shopping list of needed items from inventory

To Do:
* Fix bugs
* Save and access shopping lists

How to Use:
* Necessary files to download: househelper.R, global.R
* Your own files to create: recipes.csv, inventory.csv
* Recipes.csv must have columns: "name", "csvName", and "serves"
* Inventory.csv must have columns: "name", "stock", "need", "unit", "location", "category"
* Folder "recipes" to hold recipes

Intended Additions:
* Usage statistics: i.e commonly used ingredients and recipes
* Web-scraping from inputted URLs to create new recipes

Known Bugs:
* Duplicate recipes
* Cannot select recipe from list immediately after added
* Cannot add items not currently on inventory
* Items deleted from inventory after adding to shopping list