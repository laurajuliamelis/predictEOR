
# SHINY APP: Predict EOR

## Load libraries
library(shiny)
library(pROC)
library(ggplot2)
library(fastshap)
library(shapviz)
library(rms)
library(bslib)
library(thematic)
library(shinydashboard)
library(bsicons)

## Setup theming
bs_global_set(bs_theme(bootswatch = "flatly", base_font = font_google("Roboto")))
thematic::thematic_shiny()

## Load model
load("model_objects.Rdata")

## UI
ui <- navbarPage(
  title = "FarmaPRED-PEP: Early Onset Response (EOR) Predictor",
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Roboto"), heading_font = font_google("Poppins")),
  
  tabPanel("Prediction",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 tags$h3("Patient Inputs", style = "font-weight: bold; color: #2c3e50;"),
                 numericInput("DUP", "Duration of untreated psychosis (DUP, days)", value = 0, min = 0, max = 800),
                 numericInput("DTP", "Days of treated psychosis (DTP, days)", value = 0, min = 0, max = 400),
                 numericInput("EEAG", "Functioning score (GAF/EEAG)", value = 10, min = 10, max = 100),
                 numericInput("Reserva", "Cognitive reserve (z-score)", value = 0, step = 0.1),
                 sliderInput("Insight", "Insight (1 = good, 7 = poor)", min = 1, max = 7, value = 1, step = 1),
                 numericInput("Perseveratives", "Executive function (z-score)", value = 0, step = 0.1),
                 actionButton("predict", "Predict response", class = "btn btn-primary")
               ),
               mainPanel(
                 tags$h2("Welcome to the EOR Predictor", style = "color: #3498db; font-weight: bold;"),
                 tags$p("This application allows clinicians and researchers to estimate the probability of early onset response (EOR) to antipsychotic treatment in patients with first-episode psychosis (FEP). The data used to develop this model was the PEPS cohort from the study “Genotype-Phenotype Interaction and Environment. Application to a Predictive Model in First Psychotic Episodes” (PI08/0208), which can be found at ",
                        tags$a(href = "https://doi.org/10.1016/j.rpsm.2012.11.001", target = "_blank", "https://doi.org/10.1016/j.rpsm.2012.11.001"), "."),
                 tags$p("The inclusion criteria of the patients were: age between 7 and 35 years at the time of the first evaluation, presence of psychotic symptoms lasting less than 12 months, speaking Spanish correctly and signing the informed consent. The exclusion criteria for the patients were: mental retardation according to the criteria of the DSM-IV60 (which includes, in addition to an intellectual coefficient below 70, functional problems), a history of head trauma with loss of consciousness and organic disease with mental repercussions."),
                 p(strong("Please, use the left panel to enter your patient characteristics and view the predicted response class.")),
                 
                 tags$h2("Results", style = "color: #2980b9; font-weight: bold;"),
                 
                 tags$h4("1. Prediction"),
                 fluidRow(
                   column(7, uiOutput("class_result")),
                   column(5, uiOutput("prob_result"))
                 ),
                 
                 tags$h4("2. Performance of predicted EOR probability in your patient"),
                 plotOutput("density_plot"),
                 tags$p("This plot shows the distribution of predicted probabilities in the dataset, separated by EOR class. The vertical line represents your patient's predicted probability."),
                 
                 tags$h4("3. Variable contribution to the predicted probability"),
                 plotOutput("force_plot"),
                 tags$p("This SHAP force plot shows how each input feature contributes to increasing or decreasing the predicted probability for this specific patient.")
               )
             )
           )
  ),
  
  tabPanel("Credits and Funding",
           br(),
           tags$h3("About the FarmaPRED-PEP Project"),
           tags$p("This application was developed within the FarmaPRED-PEP projec. The FarmaPRED-PEP project aims to develop predictive models of early treatment response to antipsychotics in patients experiencing a first episode of psychosis (FEP). This work integrates clinical, neurocognitive, and environmental data to provide clinicians with accessible tools to assist in early decision-making."),
           tags$p("The project was supported by the Instituto de Salud Carlos III (ISCIII), co-funded by the European Union, and carried out by a multidisciplinary team from the University of Barcelona, IDIBAPS, and CIBERSAM."),
           tags$p("This tool is intended solely for research use and should not replace clinical judgment."),
           
           tags$h4("Contact us"),
           tags$p("For questions, comments, or feedback regarding this tool or the publication, please contact us at", tags$a(href = "mailto:sergimash@ub.edu?subject=FarmaPRED-PEP%20Tool", target = "_blank", "sergimash@ub.edu"), "and", tags$a(href = "mailto:laurajulia@ub.edu?subject=FarmaPRED-PEP%20Tool", target = "_blank", "laurajulia@ub.edu"), ". If you are interested in collaborating with us, do not hesitate to reach out!")
  ),
  
  tags$footer(
    tags$div(
      fluidRow(
        column(4, img(src = "logo_ub.png", height = "60px")),
        column(4, img(src = "logo_idibaps.png", height = "60px")),
        column(4, img(src = "logo_isciii.png", height = "60px"))
      ),
      br(),
      HTML("For further information visit <a href='https://www.google.com/' target='_blank'>the publication</a> - Code available at <a href='https://github.com/laurajuliamelis/predictEOR' target='_blank'><i class='bi bi-github'></i>GitHub</a>"),
      style = "width: 100%; color: black; text-align: center;"
    )
  )
)

## SERVER
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
      value_box(title = "Patient class", 
                value = if (prob > cutoff.clin) "EOR" else "Non-EOR" , 
                theme = value_box_theme(bg = if (prob > cutoff.clin){"#00a86b"} else {"#D22B2B"}, fg = "#FFFFFF"), 
                showcase = if (prob > cutoff.clin) bsicons::bs_icon("hand-thumbs-up-fill")else bsicons::bs_icon("hand-thumbs-down-fill"), 
                showcase_layout = "left center", 
                full_screen = F, fill = TRUE, height = NULL)
    })
    
    output$prob_result <- renderUI({
      value_box(title = "Probability of EOR", value = paste0(round(prob, 2)) , 
                theme = value_box_theme(bg = "#f4f4f4", fg = "#989898"), 
                showcase = bsicons::bs_icon("info-circle-fill"), showcase_layout = "left center", 
                full_screen = F, fill = TRUE, height = NULL)
    })
    
    data.clin.imp$PredResponse <- factor(data.clin.imp$PredResponse, levels = c(0, 1), labels = c("Non-EOR", "EOR"))
    
    output$density_plot <- renderPlot({
      ggplot(data.clin.imp, aes(x = Prediction, fill = PredResponse, color = PredResponse)) +
        geom_density(alpha = 0.2) +
        geom_vline(aes(xintercept = prob, color = "Patient"), linetype = "solid", size = 1) +
        annotate("text", x = prob + 0.04, y = 3.8, label = paste0("p=", round(prob, 3)),
                 vjust = -0.5, hjust = 0.5, color = "coral", size = 4) +
        labs(x = "Probability", fill = "Class") +
        scale_color_manual(name = NULL, values = c("Patient" = "coral")) +
        guides(color = guide_legend(override.aes = list(linetype = "solid"))) +
        theme_classic()
    })
    
    ex.clin <- explain(rms.clin, X = X, pred_wrapper = pfun, newdata = new_patient, nsim = 500, adjust = TRUE, shap_only = FALSE)
    shv <- shapviz(ex.clin)
    
    output$force_plot <- renderPlot({
      sv_force(shv, row_id = 1)
    })
  })
}
## Run the app
shinyApp(ui = ui, server = server)
