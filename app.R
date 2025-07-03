
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
bs_theme_set(bs_theme(bootswatch = "flatly", base_font = font_google("Roboto")))
thematic::thematic_shiny()

## Load model
load("model_objects.Rdata")

## Get ui.R and server.R
source("ui.R")
source("server.R")

## Run the app
shinyApp(ui = ui, server = server)
