library(shiny)
library(magrittr)

X <- read.csv("sample_data.csv")
X.dist <- dist(X)
X.stat <- summary(X.dist)[c(1,4,6)]
X.stat <- round(X.stat, digits = mean(X.stat) %>% log10 %>% ceiling %>% subtract + 2) %>% as.list
X.filt <- calcVRFilt(X, maxscale = max(X.dist), maxdimension = 2)

source("rips_complex.R")

ui <- fluidPage(
  titlePanel("App Title"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "scale", label = "radius",
                  min = X.stat$Min., max = X.stat$Max., value = X.stat$Mean)
    ),
    mainPanel(
      plotOutput("main")
    )
  )
)

server <- function(input, output, session) {
  output$main <- renderPlot(plot.filtration(X, K = filt, t = input$scale))
}

shinyApp(ui, server)
