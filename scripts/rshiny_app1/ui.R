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

