# Define server logic required to draw a rarity plot ----
checknumeric<-function(x){x=ifelse(x == "", "1,a", gsub(" ","", x))
    numtest=grepl("^[0-9]{1,}$", unlist(strsplit(x,",")))
    return(sum(numtest)==length(numtest))}

require(shiny)
source("balance.R")

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
        
        validate(
            need(checknumeric(input$abundances), "abundances must be positive integers, separated by commas")
        )
        
        # validate(need((as.numeric(unlist(strsplit(input$abundances,",")))), "uhoh"))
        # validate(need(stringr::str_detect, "[^\\d]"), "check abundances are numeric"))
        rarity_plot(as.numeric(unlist(strsplit(input$abundances,","))), input$ell, lines=input$line)
        
    })
  
    
    output$mean_rarity<-renderUI({
        validate(
            need(checknumeric(input$abundances), "")
        )
        HTML(paste("mean rarity = ",     signif(dfun(as.numeric(unlist(strsplit(input$abundances,",")))[as.numeric(unlist(strsplit(input$abundances,",")))>0], input$ell),3),sep=""), paste(", ℓ = ",  input$ell, sep=""))
    })
    
    # output$legend4app <- renderPlot({
    #     
    #     
    #     legend_for_app
    #     
    # })
    output$myEqs <- renderUI({
        withMathJax(includeMarkdown("hillNumber.md")
            )
    })
   
    
}



