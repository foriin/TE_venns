# app.R
library(shiny)
library(ggvenn)
library(plotly)

# Load the data
load("vennins.RData")
names(vennins) <- c("Tissue 1",
                    "Tissue 2",
                    "Tissue 3 Treatment",
                    "Tissue 3 Control")
# Source the functions
source("funs.R")

# Define UI for application
ui <- fluidPage(
  titlePanel("Venn Diagram of shared TE insertions"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selectedSets", "Select sets:", choices = names(vennins), selected = names(vennins))
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.selectedSets.length >= 2",
        plotlyOutput("vennPlot")
      ),
      conditionalPanel(
        condition = "input.selectedSets.length < 2",
        h3("You gotta select at least two groups!"),
        img(src = "loadcat.png", width = 333, height = 187)
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  output$vennPlot <- renderPlotly({
    # Validate the number of selected sets
    validate(
      need(length(input$selectedSets) >= 2, "You gotta select at least two groups!")
    )
    
    # Subset vennins based on user selection
    selected_vennins <- vennins[input$selectedSets]
    
    # Calculate intersections and hover texts based on selected data
    all_ix <- calculate_intersections(selected_vennins)
    hovers <- generate_hover_texts(all_ix, item_to_category)
    
    # Generate the Venn diagram
    ggvennins <- ggvenn(selected_vennins, stroke_size = 0.5, set_name_size = 4)
    ggp <- ggplotly(ggvennins)
    
    # Update hover text
    ggp$x$data[[length(selected_vennins) + 3]]$hovertext <- hovers
    print(ggp$x$data[[length(selected_vennins) + 3]]$hovertext)
    return(ggp)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
