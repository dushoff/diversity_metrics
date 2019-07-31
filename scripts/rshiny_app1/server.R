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
        
        source("balance.R")
        abus<-as.numeric(unlist(strsplit(input$abundances,",")))
        rarity_plot(abus, input$ell, lines=input$line)
        
    })
    output$legend4app <- renderPlot({
        
        source("balance.R")
        legend_for_app
        
    })
    
}



