library(shiny)
library(DT)
library(sass)
library(yaml)
library(memoise)
library(glue)

blue_light <- "#bfd7f0"
blue_medium <- "#3b5d82"

ui <- fluidPage(
  
  # shade the top tabs
  tags$head(
    tags$style(HTML(glue::glue("
      .top-tabs > .tabbable > .nav-tabs {{
        background-color: {blue_light} !important;
        padding: 6px 6px 0 6px;
        border-radius: 6px;
      }}
  ")))
  ),
  
  titlePanel("DUC2: Impact from offshore infrastructures on marine life"),
  mainPanel(
    tags$div(                 
      class = "top-tabs container-fluid",
      tabsetPanel(
        id = "tabsetPanelID",
        type = "tabs",
        
        tabPanel(
          "Home",
          fluidRow(
            column(
              width = 12,
              h3("Welcome! 👋"),
              p("Do you want to learn more about marine life and how it is influenced by offshore infrastructures? You're in the right place!"),
              h4("How to use this app"),
              p("<explanations>", style = "margin-bottom: 200px;"),
              h4("Want to learn more?"),
              p("Click the buttons below!"),
              
              tags$div(
                tags$a(
                  "More info on DUC2",
                  href = "https://dto-bioflow.eu/use-cases/duc-2-impact-offshore-infrastructures",
                  target = "_blank",
                  rel = "noopener",
                  class = "btn btn-primary d-flex align-items-center justify-content-center",
                  style = glue::glue("margin-bottom:25px;background-color:{blue_medium}; color:white;")
                ),
                HTML("<br>"),
                tags$a(
                  tagList(
                    tags$img(
                      src = "Logo_BIO-Flow2023_Final_Negative.png",
                      height = "50px",
                      style = "vertical-align:middle"
                    )
                  ),
                  href = "https://dto-bioflow.eu/",
                  target = "_blank",
                  rel = "noopener",
                  class = "btn btn-primary",
                  style = glue::glue("height:60px;background-color:{blue_medium}; color:white;")
                )
              )
            )
          )
        ),
        
        tabPanel(
          title = tagList(
            tags$img(
              src = "D_labrax_phylopic_CC0.png",
              height = "24px",
              style = "vertical-align:middle; margin-right:8px;"
            ),
            tags$span("European seabass", style = "font-size: 12px; vertical-align:middle;")
          ),
          tabsetPanel(
            tabPanel("Migration predictions", DTOutput("seabass_migration")),
            tabPanel("acoustic telemetry data", DTOutput("acoustic_telemetry")),
            tabPanel("Environmental layers", DTOutput("env_layers"))
          )
        ),
        
        tabPanel(
          title = tagList(
            tags$img(
              src = "P_phocoena_phylopic_CC0.png",
              height = "24px",
              style = "vertical-align:middle; margin-right:8px;"
            ),
            tags$span("Harbour porpoise", style = "font-size: 12px; vertical-align:middle;")
          ),
          tabsetPanel(
            tabPanel("PAM dashboard", DTOutput("PAM_dashboard")),
            tabPanel("PAM data", DTOutput("PAM_data")),
            tabPanel("Habitat suitability (Marco-Bolo)", DTOutput("HSM_porpoise"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$PAM_dashboard <- renderDT(iris)
  output$PAM_data <- renderDT(cars)
  output$HSM_porpoise <- renderDT(head(iris, 3))
}

shinyApp(ui, server)