library("shiny")
library("shinythemes")
library("dplyr")
source("helpers.R")

new_cars_data <-
  get_data() %>%
  tidy_data()

years <- new_cars_data %>%
	pull(year) %>%
	unique()

cantons <-
  new_cars_data %>%
  pull(kanton) %>%
  unique()

metrics <-
  new_cars_data %>%
  select(-kanton,-year) %>%
  colnames()

ui <- fluidPage(theme = shinytheme("yeti"),
                div(class="container",
                    titlePanel("New vehicle registrations in Switzerland"),
                    h4("Energy consumption and energy efficiency of new motor cars by canton"),
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput(
                          "year",
                          "Select year",
                          min = min(years),
                          max = max(years),
                          value = max(years),
                          step = 1,
                          sep = ""
                        ),
                        selectInput(
                          "cantons",
                          "Select Cantons to compare",
                          choices = cantons,
                          multiple = TRUE,
                          selected = c("ZH", "BE", "GE")
                        ),
                        selectInput(
                          "metric",
                          "Select Metric to compare",
                          choices = metrics,
                          selected = "avg_price"
                        )),
                      mainPanel(plotOutput("bar_plot"),
                                p("This interface provides a supplementary visualization to the ones found on the BFE website.
                                   The original documentation and resources can be found at the following URL:"),
                                a("https://www.bfe.admin.ch/bfe/en/home/supply/statistics-and-geodata/key-vehicle-data/key-data-for-new-vehicle-fleet.html")
                      ))))

server <- function(input, output, session) {
  output$bar_plot <- renderPlot({
    plot_comparison(new_cars_data,
                    input$year,
                    input$cantons,
                    input$metric)
  })

  return_time_each_second <- reactive({
    invalidateLater(1000)
    format(Sys.time(), "%a %b %d %X %Y")
  })
}

options(shiny.autoreload = TRUE)
shinyApp(ui, server)
