#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse); theme_set(theme_bw())
d_dail_app = readRDS("data/d_dail_app.rds")
d_content_app = readRDS("data/d_content_app.rds")

shinyUI(
  fluidPage(
    
    titlePanel("TV-Duell 2013: Merkel gegen Steinbrück - Dial-Messungen"),
    
    plotOutput("kurve", height = 600,
               click = "plot1_click"),
    
    hr(),
    
    fluidRow(
      column(6,
             helpText("Klicke auf die Kurve im Plot, um die detaillierte Messung im Textfeld unten anzuzeigen."),
             verbatimTextOutput("click_info")
      ),
      column(2,
             selectInput("group_factor",
                         "Gruppierung", 
                         choices = names(select(d_dail_app, -1:-4)),
                         selected = "kanzlerpraeferenz_w1"),
             uiOutput("ui")
      ),
      column(2,
             tableOutput("n_print")),
      column(2,
             radioButtons("ribbon",
                          "Variation", 
                          choices = c("Keine",
                                      "Standardabweichung",
                                      "Konfidenzintervall",
                                      "Individuelle Messungen")),
             numericInput("alpha_level",
                          HTML("&alpha;-Level"),
                          value = .05,
                          min = 0, max = 1, step = .01)
      )
    ),
    
    fluidRow(
      column(6,
             sliderInput("time",
                         "Ausschnitt aus dem TV-Duell",
                         min = min(d_dail_app$time),
                         max = max(d_dail_app$time),
                         value = c(min(d_dail_app$time), max(d_dail_app$time)),
                         timeFormat = "%H:%M:%S",
                         timezone = "+0000",
                         width = "100%",
                         step = 15)
      ),
      column(2,
             helpText('Die mit "Sprecher" und "Inhaltsanalyse" ausgewählten Kategorien aus der Inhaltsanalyse des TV-Duells lassen sich nur für einen relativ kleinen Ausschnitt auf der Zeitachse sinnvoll darstellen.')
      ),
      column(2,
             radioButtons("speaker",
                          "Sprecher", 
                          choices = c("Nein", "Ja"),
                          selected = "Nein")
      ),
      column(2,
             radioButtons("content",
                          "Inhaltsanalyse", 
                          choices = c("Nein", names(d_content_app)[-1:-3]),
                          selected = "Nein")
      )
    ),
    fluidRow(
      column(4,
        radioButtons("peak_method",
                     "Methode zur Peak-Identifikation",
                     choices = c("Keine",
                                 "Absoluter Grenzwert",
                                 "SD-Multiplikator"),
                     selected = "Keine"),
        helpText("Mit einem absoluten Grenzwert werden Peaks markiert, deren absoluter mittlerer RTR-Wert den Grenzwert überschreitet. Mit dem SD-Multiplikator werden Peaks markiert, die X Standardabweichungen vom Mittelwert der Mittelwert-Kurve entfernt liegen. Die Peaks werden in der Kurve gelb markiert und durchnummeriert.")
      ),
      column(4,
             numericInput("peak_limit",
                          "Grenzwert bzw. Multiplikator",
                          value = 1.5,
                          min = 0, max = Inf, step = .01),
             uiOutput("peak_select"),
             helpText("Durch die Auswahl eines Peaks werden zwei Minuten vor Beginn des Peaks und 30 Sekunden nach dem Peak auf der Zeitleiste ausgewählt."))
    )#,
    # fluidRow(column(12, tableOutput("chk")))
  )
)