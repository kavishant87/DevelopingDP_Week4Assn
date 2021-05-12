
library(shiny)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(splines2)
library(MASS)
library(mapproj)
shinyUI(navbarPage(
    titlePanel(title=h2("USArrests Dataset Summary",align="center")),
sidebarLayout(
    sidebarPanel(helpText("Select a crime rate to visualize data and plots"),
        selectInput("var",
                    label="Crime Box",
                    choices=c("Murder"=1,"Assault"=2,"Rape"=4),
                    selected=1)
    ),
    mainPanel(tabsetPanel(type="tabs",
                     tabPanel("DataSummary",tableOutput("Data")),
                     tabPanel("Map",plotOutput("Map")),
                     tabPanel("Linear Regression Diagnostics",plotOutput("Diagnostics"))
                          ))
)
))