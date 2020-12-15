# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
#http://shiny.rstudio.com/

library(shiny)

ui <- fluidPage(
    titlePanel("Webmapping incendies sud de france"),
    sidebarLayout(
        sidebarPanel(
            
            selectizeInput(
                inputId="options",
                label="1. Titre 1",
                choices=df_feux$dep)),
        
        dateRangeInput(
            inputId="dates",
            label=h3("Date range"),
            start="01/01/1973"),
        #hr(),
        #fluidRow(column(4, verbatimTextOutput("value")))
        ),
    mainPanel(
        helpText('Output of the examples in the left:'),
        verbatimTextOutput('ex_out'))
    )

date <- function(input, output) {
    output$value <- renderPrint({ input$dates })
    }


server <- function(input, output) {
    output$ex_out <- renderPrint({
        str(sapply(
            sprintf('e%d', 0:7),
            
            function(id) {
                input[[id]]},
            simplify=FALSE))
        })
}

shinyApp(ui=ui,
         server=server)