
library(shiny)
library(pROC)
library(ggplot2)
library(shapviz)
library(rms)

# Load model
load("model_objects.Rdata")

# UI
ui <- navbarPage(
  title = div(img(src = "logo_ub.png", height = "40px"), "FarmaPRED-PEP: Early Onset Response (EOR) Predictor"),
  theme = "style.css",

  tabPanel("Prediction",
    sidebarLayout(
      sidebarPanel(
        numericInput("DUP", "Duration of untreated psychosis (DUP, days)", value = 0, min = 0, max = 800),
        numericInput("DTP", "Days of treated psychosis (DTP, days)", value = 0, min = 0, max = 400),
        numericInput("EEAG", "Functioning score (GAF/EEAG)", value = 10, min = 10, max = 100),
        numericInput("Reserva", "Cognitive reserve (z-score)", value = 0, step = 0.1),
        sliderInput("Insight", "Insight (1 = good, 7 = poor)", min = 1, max = 7, value = 1, step = 1),
        numericInput("Perseveratives", "Executive function (z-score)", value = 0, step = 0.1),
        actionButton("predict", "Predict response")
      ),
      mainPanel(
        h2("Welcome to the FarmaPRED-PEP Predictor"),
        p("This application allows clinicians and researchers to estimate the probability of early onset response (EOR) to antipsychotic treatment in patients with first-episode psychosis (FEP)."),
        p("Use the left panel to enter patient data and view predicted classification."),
        
        h2("Results"),
        h3("1. Predicted class"),
        uiOutput("class_result"),
        h3("2. Variable contribution to the predicted probabilty"),
        plotOutput("force_plot"),
        h3("3. Probabillity density by class"),
        plotOutput("density_plot")
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
    
    output$class_result <- renderUI({
      if (prob > cutoff.clin) {
        div(paste0("Classified as EOR with a predicted probability of ", round(prob * 100, 2), "%"), style = "color: white; background-color: forestgreen; padding: 8px 10p; font-weight: bold;")
      } else {
        div(paste0("Classified as non-EOR with a predicted probability of ", round(prob * 100, 2), "%"), style = "color: white; background-color: tomato; padding: 8px; font-weight: bold;")
      }
    })
    
    output$prob_pie <- renderPlot({
      values <- c(prob, 1 - prob)
      labels <- c("EOR", "No EOR")
      colors <- if (prob > cutoff.clin) c("forestgreen", "gray80") else c("tomato", "gray80")
      pie(values,
          labels = paste0(labels, ": ", round(values * 100), "%"),
          col = colors,
          main = "Distribución de Probabilidad Predicha")
    })
    
    output$density_plot <- renderPlot({
      ggplot(data.clin.imp, aes(x = Prediction, fill = PredResponse)) +
        geom_density(alpha = 0.5) +
        geom_vline(xintercept = prob, color = "black", linetype = "dashed", size = 1.2) +
        labs(title = "Distribución de Probabilidades Predichas",
             x = "Probabilidad",
             fill = "Respuesta") +
        theme_minimal()
    })
    
    ex.clin <- explain(rms.clin, X = X, pred_wrapper = pfun, newdata = new_patient, nsim = 500, adjust = TRUE, shap_only = FALSE)
    shv <- shapviz(ex.clin)
    
    output$force_plot <- renderPlot({
      sv_force(shv, row_id = 1)
    })
  })
}


shinyApp(ui = ui, server = server)
