library(shiny)

# Define UI for app  ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Mean Rarity"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel( 
            uiOutput("myEqs")
            
            # Input: Slider for scaling exponent ell ----
            , sliderInput(inputId = "ell"
                        , label = helpText("scaling exponent", em("l"))
                        , min = -1,
                        , max = 1
                        , value = 1
                        , step = 0.2)
            , textInput(inputId="abundances"
                        , label="abundances, separated by commas"
                        , value= "20, 8, 5, 4, 2, 1")
            , checkboxInput(inputId="line"
                            , label="Plot stacks of individuals as a line segment. Check if difficult to distinguish columns; overplotting")
            , textOutput(outputId="warning")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            h3(textOutput(outputId="mean_rarity"))
            # Output: Balance plot ----
            , plotOutput(outputId = "rarityPlot")
            #and a legend as another plot
            , plotOutput(outputId="legend4app")
            
        )
    )
)

