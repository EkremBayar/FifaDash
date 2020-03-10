# Description -------------------------------------------------------------

# This R file is prepared for Leagues page.
# There are 8 leagues in this dashboard. 
# These leagues are Bundesliga, Eredivisie, La Liga, Liga Nos, Ligue 1, Premier League, Serie A and Süper Lig.
# The Leagues Page includes descriptive statistics, data tables, visualizations and comparisons. 

# Reactive

rvLeague <- reactiveValues(League = NULL)

# Observe Event League Selection
observeEvent(input$tl_select, {
  
  
  req(input$tl_league)
  
  
  rvLeague$League <- input$tl_league
  
  
  
# 1. Value Box ---------------------------------------------------------------
  
  
  output$values <- renderValueBox({
    
    if(is.null(rvLeague$League)) return(NULL)
    
    valueBox(
      color = "orange",
      value = rv$df %>% filter(League %in% rvLeague$League) %>% 
        summarise(total = paste0("€", round(sum(Values / 1000000000), digits = 1), "B")) %>% pull(total),
      subtitle = "Total League Value",
      icon=icon("landmark")
      )
    
  })
  output$numofplayers <- renderValueBox({
    
    if(is.null(rvLeague$League)) return(NULL)
    
    valueBox(color = "orange",
             value = rv$df %>% filter(League %in% rvLeague$League) %>% select(Club) %>% nrow(),
             subtitle = "Number of Players",
             icon=icon("portrait")
    )
  })
  
  output$teams <- renderValueBox({
    
    if(is.null(rvLeague$League)) return(NULL)
    valueBox(
      color = "orange",
      value = rv$df %>% filter(League %in% rvLeague$League) %>% select(Club) %>% distinct() %>% nrow(),
      "Number of Teams",
      icon=icon("project-diagram")
    )
  })
  
  
# 2. Best Team ---------------------------------------------------------------
  
  bestLeague <- rv$df %>% filter(League %in% rvLeague$League)
  
  output$best_team <- renderTable({
    
    if(!is.data.frame(bestLeague)) return(NULL)
    
    leagueTeam <- best_team(bestLeague, input = input$tl_tactic)
    
    leagueTeam %>% 
      add_row(Name = "Average Overall:", Overall = round(mean(leagueTeam$Overall), digits = 2), Position = "ALL") %>% 
      mutate(Jersey.Number = as.character(Jersey.Number),
             Club = as.character(Club),
             Jersey.Number = if_else(is.na(Jersey.Number), "", Jersey.Number),
             Club = if_else(is.na(Club), "", Club)) %>% 
      rename_all(funs(gsub("[[:punct:]]", " ", .)))
  })
  
  
# 3. Map ---------------------------------------------------------------------
  
  output$league_nat1 <-  renderPlotly({
    
    world_map <- map_data("world")
    
    numofplayers <- world_map %>% 
      mutate(region = as.character(region)) %>% 
      left_join((rv$df %>% mutate(Nationality = as.character(Nationality),
                                  Nationality = if_else(Nationality %in% "England", "UK", Nationality)) %>%
                   filter(League == rvLeague$League) %>%
                   count(Nationality, name = "Number of Player") %>%
                   rename(region = Nationality) %>%
                   mutate(region = as.character(region))), by = "region")
    
    ggplotly(
      ggplot(numofplayers, aes(long, lat, group = group))+
        geom_polygon(aes(fill = `Number of Player` ), color = "white", show.legend = FALSE)+
        scale_fill_viridis_c(option = "C")+
        theme_void()+
        labs(fill = "Number of Players",
             title = "Nationality of The Players in The League"))
    
  })
  
  
# 4. Player Value -------------------------------------------------------------------
  
  output$league_values <- renderPlot(width = "auto",{
    
    if(is.null(rvLeague$League)) return(NULL)
    
    rv$df %>% 
      filter(League == rvLeague$League) %>% 
      arrange(-Values) %>% 
      group_by(Class) %>% 
      top_n(n = 10, wt = Values) %>% 
      ggplot(aes(reorder(Name, Values), Values, fill = Class, label = paste0("€", Values / 1000000, "M")))+
      geom_col(show.legend = FALSE)+
      geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
      coord_flip()+
      theme_minimal()+
      facet_wrap(Class~., scales = "free")+
      scale_fill_manual(values = c("seagreen", "orchid", "steelblue" ,"khaki"))+
      theme(axis.text.x=element_blank(),
            strip.background =element_rect(fill="gray"),
            strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic"))+
      labs(title = "Most Valuable Players", x = NULL, y = NULL)
    
    
    
  }, height = function() {
    session$clientData$output_league_values_width})
  
  
# 5. Comprasion --------------------------------------------------------------
  
  output$league_comp1 <- renderPlot({
    
    if(is.null(rvLeague$League)) return(NULL)
    
    if(input$comp_league == "League" && input$comp_graph == "Bar"){
      
      rv$df %>% 
        group_by(League) %>% 
        summarise(Total.Value = sum(as.integer(Values), na.rm = TRUE)) %>% 
        ggplot(aes(reorder(League, Total.Value), Total.Value, fill = Total.Value))+
        geom_col(show.legend = FALSE)+
        coord_flip()+
        theme_minimal()+
        labs(x = NULL, y = "League Total Value")+
        scale_fill_gradient(low = "khaki", high = "seagreen")+
        theme(axis.line.y = element_line(colour = "darkslategray"),
              axis.ticks.x = element_line(colour = "darkslategray"))+
        scale_y_continuous(labels = c("0 €", "2 Billion €", "4 Billion €", "6 Billion €"))
      
      
    }else if(input$comp_league == "Team" && input$comp_graph == "Bar"){
      
      p <- rv$df %>%
        filter(League == rvLeague$League) %>% 
        group_by(Club) %>% 
        summarise(Total.Value = sum(Values)) %>% 
        ggplot(aes(reorder(Club, Total.Value), Total.Value, fill = Total.Value))+
        geom_col(show.legend = FALSE)+
        coord_flip()+
        theme_minimal()+
        labs(x = NULL, y = "Team Total Value")+
        scale_fill_gradient(low = "khaki", high = "seagreen")+
        theme(axis.line.y = element_line(colour = "darkslategray"),
              axis.ticks.x = element_line(colour = "darkslategray"))
      
      if(rvLeague$League %in% c("Bundesliga", "Serie A")){
      
        p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M", "")) # Bundesliga & Serie A 
        
      }else if(rvLeague$League == "Ligue 1"){ 
        
        p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M")) # Ligue 1
        
      }else if(rvLeague$League == "La Liga"){
        
        p+scale_y_continuous(labels = c("€0", "€250M", "€500M", "€750M", "")) # "La Liga"
        
      }else if(rvLeague$League == "Süper Lig"){
        
        p+scale_y_continuous(labels = c("€0", "€50M", "€100M", "€150M", "")) # Süper Lig
        
      }else if(rvLeague$League == "Liga Nos"){
        
        p+scale_y_continuous(labels = c("€0", "€100M", "€200M", "€300M", "")) # Liga Nos
        
      }else if(rvLeague$League == "Eredivisie"){
        
        p+scale_y_continuous(labels = c("€0", "€50M", "€100M", "€150M", "€200M", "€250M")) # Eredivisie
        
      }else if(rvLeague$League == "Premier League"){
        
        p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M", "€800M")) # Premier League
        
      }else{return(NULL)}

    }else if(input$comp_league == "Position" && input$comp_graph == "Bar"){
      
      p <- rv$df %>%
        filter(League == "Eredivisie") %>% 
        group_by(Class) %>% 
        summarise(Total.Value = sum(Values)) %>% 
        ggplot(aes(reorder(Class, Total.Value), Total.Value, fill = Total.Value))+
        geom_col(show.legend = FALSE)+
        coord_flip()+
        theme_minimal()+
        labs(x = NULL, y = "Position Total Value")+
        scale_fill_gradient(low = "khaki", high = "seagreen")+
        theme(axis.line.y = element_line(colour = "darkslategray"),
              axis.ticks.x = element_line(colour = "darkslategray"))
      
      if(rvLeague$League %in% c("Bundesliga", "Serie A", "Ligue 1")){
        
        p+scale_y_continuous(labels = c("€0", "€500M", "€1B", "€1.5B", "")) # Bundesliga % Serie A
        
      }else if(rvLeague$League == "La Liga"){
        
        p+scale_y_continuous(labels = c("€0", "€500M", "€1B", "€1.5B", "€2B", "")) # La Liga
        
      }else if(rvLeague$League == "Süper Lig"){
        
        p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M", ""))# Süper Lig
        
      }else if(rvLeague$League == "Liga Nos"){
        
        p+scale_y_continuous(labels = c("€0", "€200M", "€400M", "€600M", "€800M"))# Liga Nos
        
      }else if(rvLeague$League == "Eredivisie"){
        
        p+scale_y_continuous(labels = c("€0", "€100M", "€200M", "€300M", "€400M"))# Eredivisie
          
      }else if(rvLeague$League == "Premier League"){
        
        p+scale_y_continuous(labels = c("€0", "€1.4B", "€2B", "")) # Premier League
        
      }else if(rvLeague$League == "Ligue 1"){
        
        scale_y_continuous(labels = c("€0", "€500M", "€1B", "")) # Ligue 1
        
      }else{return(NULL)}
      
    
    }else if(input$comp_league == "League" && input$comp_graph == "Boxplot"){
      
      value_wage(rv$df, variable = "Value", x = "League")

      
    }else if(input$comp_league == "Team" && input$comp_graph == "Boxplot"){
      
      value_wage(rv$df %>% filter(League == rvLeague$League), variable = "Value", x = "Club")
      
    }else if(input$comp_league == "Position" && input$comp_graph == "Boxplot"){
      
      rv$df %>% filter(League == rvLeague$League) %>% 
        ggplot(aes(reorder(Position, Values), Values, fill = Class))+
        geom_boxplot()+
        coord_flip()+
        theme_minimal()+
        labs(x = NULL, y = "Position Value Range")+
        scale_fill_viridis_d(option = "E")+
        theme(axis.line.y = element_line(colour = "darkslategray"),
              axis.ticks.x = element_line(colour = "darkslategray"),
              legend.position = "bottom")
      
    }else{return(NULL)}
    
  })
  

  

}) # ObserveEvent sonu