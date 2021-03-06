library(shiny)

# Define UI for app  ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Mean Rarity"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Slider for scaling exponent ell ----
            sliderInput(inputId = "ell"
                        , label = "scaling exponent \"ell\""
                        , min = -3,
                        , max = 2
                        , value = 1
                        , step = 0.02)
            , textInput(inputId="abundances"
                        , label="abundances, separated by commas"
                        , value= "20, 8, 5, 4, 2, 1")
            , checkboxInput(inputId="line"
                            , label="Plot stacks of individuals as a line segment. Check if difficult to distinguish columns; overplotting")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Balance plot ----
            plotOutput(outputId = "rarityPlot")
            
        )
    )
)

# Define server logic required to draw a rarity plot ----
server <- function(input, output) {
       
            # rarity plot for sample community ----
            # with user-chosen scaling exponent, input abundances, and choice of lines or boxes
            # This expression that generates a rarity plot is wrapped in a call
            # to renderPlot to indicate that:
            #
            # 1. It is "reactive" and therefore should be automatically
            #    re-executed when inputs change
            # 2. Its output type is a plot
            output$rarityPlot <- renderPlot({
            
            source("scripts/balance.R")
            abus<-as.numeric(unlist(strsplit(input$abundances,",")))
            rarity_plot(abus, input$ell, lines=input$line)
        
            })
    
}

shinyApp(ui = ui, server = server)
