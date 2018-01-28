# ===================================================================
# Title: House Helper
# Description:
#   This script creates a word cloud
# Input(s): .txt files
# Output(s): shiny app word cloud
# Author: Timothy Tan
# Date: 11-29-2017
# ===================================================================

library(shiny)

source('global.R')

ui <- fluidPage(
  titlePanel("House Helper"),
  
  sidebarLayout(
    sidebarPanel(
      #Upload Inventory
      # conditionalPanel(condition="input.conditionedPanels == 'Upload'",
      #                  fileInput("file1", "Choose CSV File",
      #                            multiple = TRUE,
      #                            accept = c("text/csv",
      #                                       "text/comma-separated-values,text/plain",
      #                                       ".csv"))
      # 
      # ),
      conditionalPanel(condition="input.conditionedPanels == 'New Recipe'",       
                       textInput("title", label = h4("Title")),
                       numericInput("serves", label = h5("Serves"), value = 1, min = 1),
                       selectInput("ingredient", label = h5("Ingredient"), 
                                   choices = str_to_title(getInventory()$name)),
                       numericInput("num", label = h5("Amount"), value = 1, min = 0),
                       selectInput("unit",label = h5("Unit"), choices = sort(getInventory()$unit), 
                                   selected = 'count'),
                       actionButton("add", "Add Ingredient"),
                       actionButton("undo", "Undo"),
                       hr(),
                       actionButton("clear", "Clear"),
                       actionButton("save", "Save")
                       
      ),
      conditionalPanel(condition="input.conditionedPanels == 'Choose Recipe'",       
                       selectInput("selectRecipe", label = h4("Recipe"), 
                                   choices = getRecipeList()),
                       numericInput("scale", label = h5("Serves"), value = 1, min = 1),
                       actionButton("choose", "Select Recipe"),
                       hr(),
                       actionButton("send", "Send to Shopping List")
      ),
      conditionalPanel(condition="input.conditionedPanels == 'Add to Shopping List'",
                       textInput("request", label = h4("Requests")),
                       numericInput("reqNum", label = h5("Quantity"), value = 1, min = 0),
                       textInput("reqUnit", label = h5("Unit")),
                       actionButton("reqAdd", "Add Request"),
                       actionButton("reqUndo", "Undo"),
                       hr(),
                       actionButton("reqFinal", "Finalize Requests")
      ),
      conditionalPanel(condition="input.conditionedPanels == 'Shopping List'",
                       dateInput("date", "Date"),
                       hr(),
                       actionButton("saveShop", "Save"))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      #Upload Inventory
      tabsetPanel(
        # tabPanel("Upload", 
        #          tableOutput("contents")),
        tabPanel("New Recipe", 
                 htmlOutput("recipeTitle"),
                 tableOutput("newrecipe")),
        tabPanel("Choose Recipe",
                 htmlOutput("chooserecipeTitle"),
                 htmlOutput("chooserecipeServes"),
                 tableOutput("chooserecipe")),
        tabPanel("Add to Shopping List",
                 tableOutput("addShopList")),
        tabPanel("Shopping List",
                 htmlOutput("shopListDate"),
                 tableOutput("shoppingList")),
        id = "conditionedPanels"
      )
      
    )
  )
)

server <- function(input, output) {
  
  # #upload your inventory
  # output$contents <- renderTable({
  #   req(input$file1)
  #   df <- read.csv(input$file1$datapath)
  # return(df)
  # })
  
  #add new recipes
  output$recipeTitle <- renderUI({ h3(input$title) })
  ing <- reactiveValues()
  ing$df <- data.frame(Ingredient = character(0), Amount = numeric(0), 
                       Unit = character(0), 
                       stringsAsFactors = FALSE)
  newEntry <- observe({
    if(input$add > 0) {
      isolate(ing$df[nrow(ing$df) + 1,] <- c(input$ingredient, input$num, input$unit))
      isolate(ing$df$Amount <- as.numeric(ing$df$Amount))
    }
  })
  deleteEntry <- observe({
    if(input$undo > 0) {
      ing$df <- isolate(ing$df[-nrow(ing$df), ])
    }
  })
  clear <- observe({
    if(input$clear > 0) {
      ing$df <- data.frame(Ingredient = character(0), Amount = numeric(0), 
                           Unit = character(0),
                           stringsAsFactors = FALSE)
    }
  })
  save <- observe({
    if(input$save > 0) {
      saveNewRecipe(ing$df, input$title, input$serves)
      showModal(modalDialog(
        "Recipe Saved!"
      ))
    }
  })
  output$newrecipe <- renderTable({ing$df})
  
  #choose from existing recipes
  
  rec <- reactiveValues()
  rec$df <- data.frame(Ingredient = character(0), Amount = numeric(0), 
                       Unit = character(0),
                       stringsAsFactors = FALSE)
  rec$rtitle <- "Recipe"
  rec$servesString <- "Serves: "
  updateRecipe <- observe({
    if(input$choose > 0) {
      isolate(rec$rtitle <- input$selectRecipe)
      isolate(rec$df <- getRecipe(input$selectRecipe))
      isolate(rec$df$Amount <- (rec$df$Amount/getServesCount(as.character(input$selectRecipe)))
                                * input$scale)
      isolate(rec$servesString <- paste("Serves: ", input$scale, sep = ''))
    }
  })
  output$chooserecipeTitle <- renderUI({h3(rec$rtitle)})
  output$chooserecipeServes <- renderUI({h5(rec$servesString)})
  output$chooserecipe <- renderTable({rec$df})
  
  #add to shopping list
  addSL <- reactiveValues()
  addSL$df <- data.frame(Ingredient = character(0), Amount = numeric(0), Unit = character(0),
                         stringsAsFactors = FALSE)
  sendAddSL <- observeEvent(input$send, {
    for (i in 1:nrow(rec$df)) {
      isolate(addSL$df[nrow(addSL$df) + 1,] <- c(rec$df$Ingredient[i], rec$df$Amount[i],
                                                 rec$df$Unit[i]))
      isolate(addSL$df$Amount <- as.numeric(addSL$df$Amount))
    } 
    showModal(modalDialog("Recipe Sent!"))
  })
  
  newAddShopList <- observeEvent(input$reqAdd, {
    isolate(addSL$df[nrow(addSL$df) + 1,] <- c(input$request, input$reqNum, input$reqUnit))
  })
  undoShopList <- observe({
    if(input$reqUndo > 0) {
      addSL$df <- isolate(addSL$df[-nrow(addSL$df), ])
    }
  })
  output$shopListDate <- renderUI({h3(input$date)})
  output$addShopList <- renderTable({addSL$df})
  
  
  
  #update shopping list
  sL <- reactiveValues()
  sL$df <- data.frame(Ingredient = character(0), Amount = numeric(0), Unit = character(0),
                      Location = character(0),
                      stringsAsFactors = FALSE)
  makeSL <- observeEvent(input$reqFinal, {
    sL$df <- createShoppingList(addSL$df)
    #sL$df <- addSL$df
    showModal(modalDialog(
      "Shopping List Saved!"
    ))
  })
  output$shoppingList <- renderTable({sL$df})
  
}

shinyApp(ui = ui, server = server)