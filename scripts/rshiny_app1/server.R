# Define server logic required to draw a rarity plot ----
checknumeric<-function(x){tryCatch(is.numeric(as.numeric(unlist(strsplit(x,",")))^2), error=function(e)e)}

server <- function(input, output, session) {
    
   
    
    # rarity plot for sample community ----
    # with user-chosen scaling exponent, input abundances, and choice of lines or boxes
    # This expression that generates a rarity plot is wrapped in a call
    # to renderPlot to indicate that:
    #''
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs change
    # 2. Its output type is a plot

# abus <- renderTable({
#         # validate(
#         #     need(is.vector(c(input$abundances)))
#         # )
#     input$abundances
#     # as.numeric(unlist(strsplit(input$abundances,",")))
#     })
    
    output$rarityPlot <- renderPlot({
        # validate(checknumeric(input$ell))
        # validate(need((as.numeric(unlist(strsplit(input$abundances,",")))), "uhoh"))
       
        rarity_plot(as.numeric(unlist(strsplit(input$abundances,","))), input$ell, lines=input$line)
        
    })
  
    
    output$mean_rarity<-renderText({
        paste("mean rarity = ",     signif(dfun(as.numeric(unlist(strsplit(input$abundances,","))), input$ell),3), "; ell = ", input$ell, sep="")
    })
    
    output$legend4app <- renderPlot({
        
        source("balance.R")
        legend_for_app
        
    })
    output$myEqs <- renderUI({
        withMathJax(
            helpText( 'Hill diversity can be expressed as the mean species rarity: $$D=[ \\sum_{i=1}^S p_i(r_i)^l]^\\frac{1}{l}$$')
            , helpText("where", em("S"),' = total number of species,')
            # , helpText('$$p_i= \\frac{abundance_i}{\\sum_{i=1}^Sabundance_i}$$')
            , helpText(em("p_i"), "= relative abundance of species", em("i"),",")
            , helpText(em("r_i"), "= rarity  of species", em("i"), "; 1/", em("p_i"),",")
            , helpText("and", em("l"),' is the control parameter that scales the rarity axis.')
            , helpText('In this program,', em("D"),' is shown as the fulcrum of the lever. The arithmetic, geometric, and harmonic means are included as reference points for each scale.')
            )
    })
   
    
}



