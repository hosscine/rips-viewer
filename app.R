library(shiny)

X <- read.csv("sample_data.csv")
source(rips_complex.R)

ui <- fluidPage(
  titlePanel("App Title"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      plotOutput("main")
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
