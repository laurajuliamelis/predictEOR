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