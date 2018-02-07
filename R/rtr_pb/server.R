#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(tidyverse); theme_set(theme_bw(base_size = 14))
d_pb_app = readRDS("data/d_pb_app.rds")
n_d = d_pb_app %>% group_by(match_id) %>% slice(1)
d_content_app = readRDS("data/d_content_app.rds")

party_colors = scale_color_manual(values = c("Angela Merkel" = "black", "Peer Steinbrueck" = "red", "keinen von beiden" = "orange", "CDU/CSU" = "black", "AfD" = "blue", "alle" = "black", "DIE LINKE" = "purple", "FDP" = "yellow", "GRUENE" = "green", "keine Angabe" = "darkgrey", "keine Partei" = "orange", "maennlich" = "darkblue", "noch nicht entschieden" = "orange", "PIRATEN" = "goldenrod", "SPD" = "red", "trifft nicht zu" = "darkgrey", "weiblich" = "green"))
party_fills = scale_fill_manual(values = c("Angela Merkel" = "black", "Peer Steinbrueck" = "red", "keinen von beiden" = "orange", "CDU/CSU" = "black", "AfD" = "blue", "alle" = "black", "DIE LINKE" = "purple", "FDP" = "yellow", "GRUENE" = "green", "keine Angabe" = "darkgrey", "keine Partei" = "orange", "maennlich" = "darkblue", "noch nicht entschieden" = "orange", "PIRATEN" = "goldenrod", "SPD" = "red", "trifft nicht zu" = "darkgrey", "weiblich" = "pink", "Merkel" = "black", "Merkel, Angela" = "black", "Steinbrueck" = "red", "Steinbrueck, Peer" = "red", "Moderator/in" = "turquoise", "Between-Saldo" = "black"))


shinyServer(function(input, output) {
  
  output$ui <- renderUI({
    checkboxGroupInput("groups",
                       "Gruppen",
                       choices = unique(unlist(d_pb_app[, input$group_factor])),
                       selected = c("Angela Merkel", "Peer Steinbrueck"))
  })
  
  visdata <- reactive({
    d = d_pb_app %>%
      filter_at(vars(one_of(input$group_factor)), any_vars(. %in% input$groups)) %>%
      group_by_at(vars(one_of(input$group_factor))) %>% 
      mutate(n = length(unique(match_id))) %>% ungroup %>%  
      group_by_at(vars(one_of(input$aggr_time, input$group_factor), cand, valence)) %>% 
      summarise(SUM_rtr = mean(trans_rtr), n = mean(n)) %>%
      ungroup %>% 
      mutate(SUM_rtr = SUM_rtr / n)
    
    if (input$aggr_item == 2) {
      d %>% 
        group_by_at(vars(one_of(input$aggr_time, input$group_factor), cand)) %>%
        summarise(SUM_rtr = sum(SUM_rtr))
    } else if (input$aggr_item == 3) {
      d  %>% 
        group_by_at(vars(one_of(input$aggr_time, input$group_factor), cand)) %>%
        summarise(SUM_rtr = sum(SUM_rtr)) %>% 
        ungroup %>% 
        mutate(SUM_rtr = ifelse(cand == "Merkel", SUM_rtr, -SUM_rtr)) %>% 
        group_by_at(vars(one_of(input$aggr_time, input$group_factor))) %>% 
        summarise(SUM_rtr = sum(SUM_rtr)) %>%
        mutate(cand = "Between-Saldo")
    }
      else {
        d
      }
  })
  
  n_table = reactive({
    n_d %>% group_by_at(vars(one_of(input$group_factor))) %>% count()
  })
  
  output$n_print = renderTable({
    n_table()
  })
  
  output$kurve <- renderPlot({
    min_rtr = visdata() %>% .$SUM_rtr %>% min
    
    p = visdata() %>%
      ggplot(aes_string(input$aggr_time, "SUM_rtr"))
    
    if (input$speaker == "Ja") {
      p = p +
        geom_rect(data = d_content_app,
                  aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf,
                      fill = sprecher),
                  inherit.aes = FALSE, alpha = .2)
    }

    if (input$aggr_item != 3) {
    p = p + geom_bar(aes(fill = cand), stat = "identity", position = "dodge") + 
      facet_wrap(input$group_factor, ncol = 1)
    } else {
      p = p + geom_line(aes_string(color = input$group_factor))
    }

    p = p + coord_cartesian(xlim = input$time) +
      labs(color = NULL, fill = NULL, x = NULL) + 
      geom_hline(yintercept = 0, linetype = 2) + 
      party_colors + party_fills
    
    if (input$content != "Nein") {
      p = p + geom_text(data = d_content_app, aes(start_time, min_rtr - 0.1 * abs(min_rtr), label = eval(parse(text = input$content))), angle = 90, vjust = 1, hjust = 0, inherit.aes = FALSE)
    }
    
    p
  })
  
  output$click_info <- renderPrint({
    nearPoints(visdata(), input$plot1_click, addDist = FALSE, maxpoints = 3)
  })
  
})
