library(shinydashboard)
source("functioncode.R")

ui <- dashboardPage(
  dashboardHeader(title = paste("Simulation of Xbar and ", expression(theta) ), titleWidth = 500),
  dashboardSidebar(disable=TRUE),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 500)),

      box(
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 3,
        background = "black",
        title = "Sampling parameters",
        numericInput("nnumber", "Sample size", 50),
        numericInput("iternumber", "Iterations", 1000)
      ),
      box(
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 3,
        background = "lime",
        title = "Population means",
        numericInput("mu1", "mu 1", 0),
        numericInput("mu2", "mu 2", 0)
      ),
      box(
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 3,
        background = "lime",
        title = "Population (co)variances",
        numericInput("s11", "variance x1", 100),
        numericInput("s12", "covariance x1,x2", 40),
        numericInput("s22", "variance x2", 400)

      ),
      box(
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 3,
        background = "black",
        title = "Linear combinations",
        numericInput("c1", "Coefficient 1", 1),
        numericInput("c2", "Coefficient 2", 0)

      )
    )
  )
)



server <- function(input, output) {


  output$plot1 <- renderPlot({



    mat = matrix(c(input$s11,input$s12,input$s12, input$s22), nr=2,nc=2)
    mui = c(input$mu1,input$mu2)
    con = c(input$c1,input$c2)

    xbarthetadist(n=input$nnumber, iter=input$iternumber, mu = mui, sigma=mat, con=con)
    })

}

shinyApp(ui, server)


