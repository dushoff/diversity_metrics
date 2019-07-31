library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Mean Rarity Balance plots"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Slider for scaling exponent ell ----
            sliderInput(inputId = "ell"
                        , label = "scaling exponent \"l\":"
                        , min = -5,
                        , max = 2
                        , value = 1
                        , step = 0.1)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Balance plot ----
            plotOutput(outputId = "rarityPlot")
            
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
            # l<-reactive({
            #     req(input$ell)
            #     input$ell
            # })
            # rarity plot for sample community ----
            # with user-chosen scaling exponent 
            # This expression that generates a histogram is wrapped in a call
            # to renderPlot to indicate that:
            #
            # 1. It is "reactive" and therefore should be automatically
            #    re-executed when inputs (input$bins) change
            # 2. Its output type is a plot
            output$rarityPlot <- renderPlot({
            
            source("scripts/balance.R")
            rarity_plot(ab, input$ell)
        
            })
    
}

shinyApp(ui = ui, server = server)
