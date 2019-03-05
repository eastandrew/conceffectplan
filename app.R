#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Constant Factor Concentration and Effect Planning"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("effect",
                     "Magnitude of Effect:",
                     min = 0,
                     max = 1,
                     value = 0.5),
         sliderInput("numT",
                     "Number of Treatments:",
                     min = 1,
                     max = 20,
                     value = 4),
         numericInput("startconc", label=h3("Starting Concentration"), value=0.05),
         numericInput("changefactor", label = h3("Change Factor"), value = exp(1))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("scatterPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$scatterPlot <- renderPlot({
     effectmag <- input$effect
     numtreats <- input$numT
     startinglow <- input$startconc
     factorstart <- input$changefactor
     
     df <- data.frame(effmag=seq(1,1-effectmag,length.out=numtreats))
     df$treatstart <- startinglow
     df$factor <- factorstart
     df$ID <- as.numeric(row.names(df))-1
     df$treat <- df$treatstart*(df$factor^df$ID)
     #df
     plot(1,1,pch=NA, ylim=c(0,1),xlim=c(min(df$treat),max(df$treat)),xlab="log visualized conc",ylab="effect",log="x")
     points(effmag~treat, data=df, pch=NA, type="b")
     text(effmag~treat, data=df, bquote(.(round(df$treat,3))))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

