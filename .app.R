# Load required packages
library(shiny)
library(ggvenn)

#Load vennins dataset
load("vennins.RData")
# Load functions
source("funs.R")

# Shiny UI
ui <- fluidPage(
  titlePanel("Venn Diagrams of shared active TE insertions"),
  
  sidebarLayout(
    sidebarPanel(
      # Checkbox group for selecting subsets
      checkboxGroupInput("selectedSubsets", "Select Subsets:", 
                         choices = names(vennins), 
                         selected = names(vennins))
    ),
    
    mainPanel(
      # Display the Venn diagram
      plotOutput("vennPlot")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  # Render Venn diagram based on selected subsets
  output$vennPlot <- renderPlot({
    selected_subsets <- input$selectedSubsets
    if (length(selected_subsets) > 0) {
      ggvenn(vennins[selected_subsets], stroke_size = 0.5, set_name_size = 4)
    }
  })
}

# Run the app
shinyApp(ui, server)
