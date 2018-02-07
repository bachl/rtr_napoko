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
d_pb_app = readRDS("data/d_pb_app.rds")
d_content_app = readRDS("data/d_content_app.rds")

shinyUI(
  fluidPage(
    
    titlePanel("TV-Duell 2013: Merkel gegen Steinbrück - Push-Button-Messungen (Nur Video-Stimulus)"),
    
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
                         choices = names(d_pb_app)[6:10],
                         selected = "kanzlerpraeferenz_w1"),
             uiOutput("ui")
      ),
      column(2,
             tableOutput("n_print")),
      column(1,
             radioButtons("aggr_item",
                          "Aggregation Items", 
                          choices = c("Keine (negativ & positiv für beide Kandidaten)" = 1,
                                      "Within-Kandidaten-Saldo (Kandidat positiv - Kandidat negativ)" = 2,
                                      "Between-Kandidaten-Saldo (Saldo Merkel - Saldo Steinbrück)" = 3))
      ),
      column(1,
             radioButtons("aggr_time",
                          "Aggregation Zeit", 
                          choices = c("1 Sekunde" = "time",
                                      "5 Sekunden)" = "time05",
                                      "10 Sekunden" = "time10",
                                      "20 Sekunden" = "time20",
                                      "30 Sekunden" = "time30",
                                      "1 Minute" = "time60"),
                          selected = "time20")
      )
    ),
    
    fluidRow(
      column(6,
             sliderInput("time",
                         "Ausschnitt aus dem TV-Duell",
                         min = min(d_pb_app$time),
                         max = max(d_pb_app$time),
                         value = c(min(d_pb_app$time), max(d_pb_app$time)),
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
    )
  )
)