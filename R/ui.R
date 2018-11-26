library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Hello Shiny!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput("axis1",
                 h5("PCA axis 1: "),
                 min=1, max=4, value=1),
      sliderInput("axis2",
                 h5("PCA axis 2: "),
                 min=2, max=4, value=2)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      tabsetPanel(
        type = "tabs",
        id = "navbar",
        tabPanel("Samples plot",
                 plotOutput("samplePlot"),
                 actionButton("summary_save","Save"))
      )

    )
  )
)
