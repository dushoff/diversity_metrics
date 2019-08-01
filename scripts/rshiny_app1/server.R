# Define server logic required to draw a rarity plot ----
server <- function(input, output, session) {

    # rarity plot for sample community ----
    # with user-chosen scaling exponent, input abundances, and choice of lines or boxes
    # This expression that generates a rarity plot is wrapped in a call
    # to renderPlot to indicate that:
    #''
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs change
    # 2. Its output type is a plot

    
    output$rarityPlot <- renderPlot({
       
        rarity_plot(as.numeric(unlist(strsplit(input$abundances,","))), input$ell, lines=input$line)
        
    })
  
    
    output$mean_rarity<-renderText({
        paste("mean rarity = ",     dfun(as.numeric(unlist(strsplit(input$abundances,","))), input$ell), "; ell = ", input$ell, sep="")
    })
    
    output$legend4app <- renderPlot({
        
        source("balance.R")
        legend_for_app
        
    })
    output$myEqs <- renderUI({
        withMathJax(
            helpText( 'Hill diversity can be expressed as the mean species rarity: $$D=\\sum_{i=1}^S [p_i(1/p_i)^l]^\\frac{1}{l}$$')
            , helpText("where", em("S"),' = total number of species')
            , helpText('and $$p_i= \\frac{abundance_i}{\\sum_{i=1}^Sabundance_i}$$')
            , helpText("and", em("l"),' is the control parameter that scales the rarity axis.')
            , helpText('In this program,', em("D"),' is shown as the fulcrum of the lever. The arithmetic, geometric, and harmonic means are included as reference points for each scale.')
            )
    })
   
    
}



