
library(shiny)
library(rms)

# Load model
rms.clin <- readRDS("rms.clin.RDS")

# UI
ui <- navbarPage(
  title = div(img(src = "logo_ub.png", height = "40px"), "FarmaPRED-PEP: Early Response Predictor"),
  theme = "style.css",

  tabPanel("Home",
    fluidPage(
      h2("Welcome to the FarmaPRED-PEP Predictor"),
      p("This application allows clinicians and researchers to estimate the probability of early onset response (EOR) to antipsychotic treatment in patients with first-episode psychosis (FEP)."),
      p("The model is based on clinical data and was developed within the FarmaPRED-PEP project, funded by Instituto de Salud Carlos III and the European Union."),
      p("Use the Prediction tab to enter patient data and view predicted probability.")
    )
  ),

  tabPanel("Prediction",
    sidebarLayout(
      sidebarPanel(
        numericInput("DUP", "Duration of untreated psychosis (DUP, days)", value = 61, min = 0, max = 800),
        numericInput("DTP", "Days of treated psychosis (DTP, days)", value = 22, min = 0, max = 400),
        numericInput("EEAG", "Functioning score (GAF/EEAG)", value = 51, min = 10, max = 100),
        numericInput("Reserva", "Cognitive reserve (z-score)", value = 0.02, step = 0.1),
        sliderInput("Insight", "Clinical insight (1 = good, 7 = poor)", min = 1, max = 7, value = 3, step = 1),
        numericInput("Perseveratives", "Perseverative responses (z-score)", value = -0.63, step = 0.1),
        actionButton("predict", "Predict response")
      ),
      mainPanel(
        h3("Prediction result"),
        verbatimTextOutput("pred_text"),
        plotOutput("prob_plot")
      )
    )
  ),

  tabPanel("Credits and Funding",
    fluidRow(
      column(4, img(src = "logo_ub.png", height = "80px")),
      column(4, img(src = "logo_idibaps.png", height = "80px")),
      column(4, img(src = "logo_isciii.png", height = "80px"))
    ),
    br(),
    p("This application was developed within the FarmaPRED-PEP project, funded by the Instituto de Salud Carlos III (ISCIII) and co-funded by the European Union."),
    p("Affiliations: University of Barcelona (UB), IDIBAPS, CIBERSAM."),
    br(),
    p(em("For research purposes only. Not intended for clinical diagnosis."))
  )
)

server <- function(input, output, session) {
  observeEvent(input$predict, {
    new_patient <- data.frame(
      DUP = input$DUP,
      DTP = input$DTP,
      EEAG_Total_VB = input$EEAG,
      Reserva_Cognitiva = input$Reserva,
      Insight = input$Insight,
      respuestas_perseverativas_PTV2M = input$Perseveratives
    )
    
    prob <- predict(rms.clin, newdata = new_patient, type = "fitted")
    
    output$pred_text <- renderText({
      paste0("Predicted probability of early response: ", round(prob * 100, 1), "%")
    })
    
    output$prob_plot <- renderPlot({
      values <- c(prob, 1 - prob)
      labels <- c("Early Response", "No Early Response")
      colors <- if (prob > 0.5) c("forestgreen", "gray80") else c("tomato", "gray80")
      pie(values,
          labels = paste0(labels, ": ", round(values * 100), "%"),
          col = colors, border="white",
          main = "Predicted Probability")
    })
  })
}


shinyApp(ui = ui, server = server)
