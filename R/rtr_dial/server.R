#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse); theme_set(theme_bw(base_size = 14))
d_dail_app = readRDS("data/d_dail_app.rds")
n_d = d_dail_app %>% group_by(match_id) %>% slice(1)
d_content_app = readRDS("data/d_content_app.rds")

party_colors = scale_color_manual(values = c("Angela Merkel" = "black", "Peer Steinbrueck" = "red", "keinen von beiden" = "orange", "CDU/CSU" = "black", "AfD" = "blue", "alle" = "black", "DIE LINKE" = "purple", "FDP" = "yellow", "GRUENE" = "green", "keine Angabe" = "darkgrey", "keine Partei" = "orange", "maennlich" = "darkblue", "noch nicht entschieden" = "orange", "PIRATEN" = "goldenrod", "SPD" = "red", "trifft nicht zu" = "darkgrey", "weiblich" = "pink"))
party_fills = scale_fill_manual(values = c("Angela Merkel" = "black", "Peer Steinbrueck" = "red", "keinen von beiden" = "orange", "CDU/CSU" = "black", "AfD" = "blue", "alle" = "black", "DIE LINKE" = "purple", "FDP" = "yellow", "GRUENE" = "green", "keine Angabe" = "darkgrey", "keine Partei" = "orange", "maennlich" = "darkblue", "noch nicht entschieden" = "orange", "PIRATEN" = "goldenrod", "SPD" = "red", "trifft nicht zu" = "darkgrey", "weiblich" = "pink", "Merkel" = "black", "Merkel, Angela" = "black", "Steinbrueck" = "red", "Steinbrueck, Peer" = "red", "Moderator/in" = "turquoise"))


shinyServer(function(input, output, session) {
  
  output$ui <- renderUI({
    checkboxGroupInput("groups",
                       "Gruppen",
                       choices = unique(unlist(d_dail_app[, input$group_factor])),
                       selected = c("Angela Merkel", "Peer Steinbrueck"))
  })
  
  visdata <- reactive({
    d = d_dail_app %>%
      filter_at(vars(one_of(input$group_factor)), any_vars(. %in% input$groups)) %>% 
      group_by_at(vars(time, one_of(input$group_factor))) %>% 
      summarise(M_rtr = mean(rtr, na.rm = TRUE),
                SD_rtr = sd(rtr, na.rm = TRUE),
                N_rtr = n()) %>%
      ungroup
    if (input$ribbon == "Standardabweichung") {
      d %>% mutate(LO_rtr = M_rtr - SD_rtr,
                   UP_rtr = M_rtr + SD_rtr)
    } else if (input$ribbon == "Konfidenzintervall") {
      d %>% mutate(SE_rtr = SD_rtr / sqrt(N_rtr),
                   LO_rtr = qt(input$alpha_level/2, df = N_rtr-1) * SE_rtr + M_rtr,
                   UP_rtr = qt(1-input$alpha_level/2, df = N_rtr-1) * SE_rtr + M_rtr)
    }
    else {
      d %>% mutate(SE_rtr = SD_rtr / sqrt(N_rtr),
                   LO_rtr = qt(input$alpha_level/2, df = N_rtr-1) * SE_rtr + M_rtr,
                   UP_rtr = qt(1-input$alpha_level/2, df = N_rtr-1) * SE_rtr + M_rtr)
    }
  })
  
  indi_visdata <- reactive({
    d = d_dail_app %>%
      filter_at(vars(one_of(input$group_factor)), any_vars(. %in% input$groups))
  })
  
  n_table = reactive({
    n_d %>% group_by_at(vars(one_of(input$group_factor))) %>% count()
  })
  
  
  peak_data = reactive({
    d = d_dail_app %>%
      filter_at(vars(one_of(input$group_factor)), any_vars(. %in% input$groups)) %>% 
      group_by_at(vars(time, one_of(input$group_factor))) %>% 
      summarise(M_rtr = mean(rtr)) %>%
      ungroup
    if (input$peak_method == "Absoluter Grenzwert") {
      d = d %>% filter(abs(M_rtr) > input$peak_limit) %>% 
        arrange_at(vars(one_of(input$group_factor), time)) %>%
        group_by_at(vars(one_of(input$group_factor))) %>%
        mutate(peak_lag = difftime(time, lag(time), "secs")) %>% ungroup
    } else if (input$peak_method == "SD-Multiplikator") {
      d = d %>% group_by_at(vars(one_of(input$group_factor))) %>%
        mutate(M_M_rtr = mean(M_rtr), SD_M_rtr = sd(M_rtr),
               loli = M_M_rtr - input$peak_limit * SD_M_rtr,
               upli = M_M_rtr + input$peak_limit * SD_M_rtr) %>% 
        filter(M_rtr > upli | M_rtr < loli) %>% ungroup %>% 
        arrange_at(vars(one_of(input$group_factor), time)) %>%
        group_by_at(vars(one_of(input$group_factor))) %>%
        mutate(peak_lag = difftime(time, lag(time), "secs")) %>% ungroup
    } else {
      d = d
    }
    ctr = 1
    if (input$peak_method != "Keine") {
      for (i in 2:nrow(d)) {
        if (d$peak_lag[i] > 5 | is.na(d$peak_lag[i])) ctr[i] = ctr[i - 1] + 1 else ctr[i] = ctr[i - 1]
      }
    }
    d %>% mutate(peak = ctr) %>%
      group_by(peak) %>%
      summarise(peak_start = min(time),
                peak_end = max(time),
                peak_med = median(time),
                peak_rtr = ifelse(abs(min(M_rtr)) > abs(max(M_rtr)), min(M_rtr), max(M_rtr)))
  })
  
  output$n_print = renderTable({
    n_table()
  })
  
  # output$chk = renderTable({
  #   peak_data()
  # })
  
  output$kurve <- renderPlot({
    
    p = visdata() %>%
      ggplot(aes(time, M_rtr, ymin = LO_rtr, ymax = UP_rtr))
    
    
    if (input$speaker == "Ja") {
      p = p +
        geom_rect(data = d_content_app,
                  aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf,
                      fill = sprecher),
                  inherit.aes = FALSE, alpha = .2)
    }
    
    if (input$peak_method != "Keine") {
      p = p + geom_rect(data = peak_data(),
                        aes(xmin = peak_start, xmax = peak_end, ymin = -Inf, ymax = Inf),
                        fill = "yellow", inherit.aes = FALSE, alpha = .5)
    }
    
    
    if (input$ribbon %in% c("Standardabweichung", "Konfidenzintervall")) {
      p = p + geom_ribbon(aes_string(fill = input$group_factor), alpha = .5)
    }
    
    if (input$ribbon == "Individuelle Messungen") {
      p = p + geom_line(data = indi_visdata(),
                        aes_string("time", "rtr", group = "match_id", color = input$group_factor),
                        inherit.aes = FALSE, alpha = .3, position = position_jitter(width = 0, height = .1),
                        size = 1)
    }
    
    p = p + geom_line(aes_string(color = input$group_factor), size = 2) + 
      scale_y_continuous("Mittlere RTR-Bewertung", limits = c(-3.1, 3.1), breaks = -3:3,
                         labels = c("Vorteil Steinbrueck", -2:2, "Vorteil Merkel")) +
      labs(color = NULL, fill = NULL, x = NULL) + 
      geom_hline(yintercept = 0, linetype = 2) + 
      coord_cartesian(xlim = input$time) +
      party_colors + party_fills
    
    if (input$content != "Nein") {
      p = p + geom_text(data = d_content_app, aes(start_time, -3.1, label = eval(parse(text = input$content))), angle = 90, vjust = 1, hjust = 0, inherit.aes = FALSE)
    }
    
    if (input$peak_method != "Keine") {
      p = p + geom_label(data = peak_data(), aes(peak_med, peak_rtr + peak_rtr * 0.1, label = peak),
                         inherit.aes = FALSE)
    }
    
    p
  })
  
  output$click_info <- renderPrint({
    nearPoints(visdata() %>% select(-LO_rtr, -UP_rtr), input$plot1_click, addDist = FALSE, maxpoints = 3)
  })
  
  output$peak_select <- renderUI({
    selectInput("peak_select",
                "Peak zur Darstellung auswählen",
                choices = c(NA, sort(unique(peak_data()$peak))),
                selected = NA)
  })
  
  observe({
    peak_times = peak_data() %>%
      filter(peak == input$peak_select) %>% 
      mutate(peak_start = peak_start - 120, peak_end = peak_end + 30)
    updateSliderInput(session,
                      "time",
                      value = c(peak_times$peak_start, peak_times$peak_end))
  })
  
})
