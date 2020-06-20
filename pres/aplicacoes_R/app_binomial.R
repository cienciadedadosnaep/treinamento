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
   titlePanel("Distribuição Binomial"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("n",
                     "Tamanho do lote:",
                     min = 0,
                     max = 1000,
                     value = 30),
#      ),
         sliderInput("p",
                    "Probab. Defeituosa:",
                     min = 0,
                     max = 1,
                     value = 0.5,
                    step=0.01)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      #bins <- seq(min(x), max(x), length.out = input$k + 1)
      
      # draw the histogram with the specified number of bins
      require(graphics)
      # Compute P(45 < X < 55) for X Binomial(100,0.5)
      p<- input$p
      #sum(dbinom(46:54, 100, 0.5))
      
      ## Using "log = TRUE" for an extended range :
      n <- input$n
      k <- seq(0, n, by = 1)
      plot (k, dbinom(k, n,p, log = FALSE), type = "h", 
      xlim=c(-1,n),
      #ylim=c(0,1),
      cex.lab = 2,cex.axis=2,lwd=3,col="blue",ylab="p",
      main = "Distribuição binomial")
      points(k, dbinom(k, n, p, log = FALSE),pch=16,cex=2,col="dark red")
           # lines(k, (dbinom(k, n, pi/10)), col = "red", lwd = 2)
      ## extreme points are omitted since dbinom gives 0.
     # mtext("dbinom(k, log=TRUE)", adj = 0)
    #  mtext("extended range", adj = 0, line = -1, font = 4)
    #  mtext("log(dbinom(k))", col = "red", adj = 1)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

