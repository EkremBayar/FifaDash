# Description -------------------------------------------------------------

# This R file is prepared for Teams page.
# The Teams Page includes descriptive statistics, data tables, visualizations. 


rvTeam<- reactiveValues(League = NULL, Team = NULL)

observeEvent(input$tt_select,{
  
  req(input$tt_league)
  req(input$tt_team)
  
  rvTeam$League <- input$tt_league
  rvTeam$Team   <- input$tt_team
  

# 1. Summary -----------------------------------------------------------------

  
  
  output$tt_summary <- renderPlot({
    
    if(is.null(rvTeam$League) | is.null(rvTeam$Team)) return(NULL)
    
    
    tile_data <- rv$df %>%
      filter(League == rvTeam$League, Club == rvTeam$Team) %>% 
      select_if(is.numeric) %>% 
      cbind("Name.Pos" = rv$df %>% 
              filter(League == rvTeam$League, Club == rvTeam$Team) %>% pull(Name.Pos)) %>% 
      mutate(Name.Pos = as.character(Name.Pos)) %>% 
      select(-contains("GK")) %>%
      left_join(rv$df %>% select(Name.Pos, Position, Class) %>% mutate(Name.Pos = as.character(Name.Pos)), by = "Name.Pos") %>%
      filter(Overall >= 70) %>%
      select(-Name.Pos, -International.Reputation, -Jersey.Number, -Skill.Moves, -Special, -Values, -Weak.Foot, 
             -Age, -Overall, -Potential, -starts_with("Wage"), -Height, -Weight)
    
    
    tile_data <- tile_data %>%
      filter(Position != "GK") %>%
      gather(key = Attribute, value = Value, -Position, -Class) %>%
      group_by(Class, Position, Attribute) %>%
      summarise(MedianValue = median(Value, na.rm = T))
    
    tile_data %>%
      ggplot(aes(x= Attribute, y= Position)) +
      geom_tile(aes(fill = MedianValue), colour = "black") +
      geom_text(aes(label = MedianValue)) +
      scale_fill_gradient(low = "purple", high = "green") +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(face = "bold", size = 12), legend.position = "none") +
      facet_wrap(~Class, scales = "free", ncol = 1)
    
  })
  


# 2.Value -------------------------------------------------------------------

  
  output$tt_value <- renderPlot({
    
    if(is.null(rvTeam$League) | is.null(rvTeam$Team)) return(NULL)
    

    grid.arrange(

      rv$df %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team) %>% arrange(-Values) %>% head(11) %>%
      ggplot(aes(reorder(Name, Values), Values, fill = Class, label = paste0("€", Values / 1000000, "M")))+
      geom_col(show.legend = FALSE, fill = "steelblue")+
      geom_text_repel(ylim = 1, segment.color = "white", color = "white")+
      coord_flip()+
      labs(title = "Most Valuable Players", x = NULL, y = NULL)+
      theme_minimal()+
      theme(axis.text.x=element_blank(),
            strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic")),

    rv$df %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team) %>%
      ggplot(aes(Values))+
      geom_histogram(fill = "steelblue")+
      labs(title = "Distribution of Player Values", x = NULL, y = NULL)+
      theme_minimal()+
      scale_x_continuous(labels = comma),

    ncol = 2)
    
 })
  
  output$tt_value2 <- renderPlot({
    
    if(is.null(rvTeam$League) | is.null(rvTeam$Team)) return(NULL)
    
    grid.arrange(
      rv$df %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team) %>% 
        ggplot(aes(x="", y = Values, fill = Class))+
        geom_boxplot()+coord_flip()+ facet_wrap(Class~.)+
        theme_minimal()+
        theme(legend.position="bottom")+ 
        scale_fill_viridis_d(option = "E")+
        scale_y_continuous(labels = comma)+
        labs(x = NULL, y = "Value", title = "Distribution of Class Values with Boxplot"),
      
      rv$df %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team) %>% 
        ggplot(aes(Values, fill = Class))+
        geom_density()+ facet_wrap(Class~.)+
        theme_minimal()+
        theme(legend.position="bottom", axis.text.y=element_blank())+ 
        scale_fill_viridis_d(option = "E")+
        scale_x_continuous(labels = comma)+
        labs(y = NULL, x = "Value", title = "Distribution of Class Values with Density"),
      ncol = 2)
  })
  
  output$tt_value3 <- renderPlotly({
    
    if(is.null(rvTeam$Team)) return(NULL)
    
    f <- rv$fifa_team_year %>% filter(club %in% rvTeam$Team) %>% 
      group_by(Year, club) %>% 
      summarise(`Total Value` = sum(value_eur))
    
    ggplotly(
      ggplot(f)+
        geom_line(aes(x = Year, y = `Total Value`, group = 1), show.legend = FALSE, color = "steelblue")+
        theme_minimal()+
        theme(legend.position='none',
              strip.background =element_rect(fill="gray"),
              strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic"))+
        labs(x = NULL, y = "Value, €", title = "Total Team Value by Years")+
        scale_y_continuous(labels = comma)
    )
    
  })
  
  
  
  
 
# 3.Best Team ---------------------------------------------------------------
  
  bestTeam <- rv$df %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team)
  
  output$tt_best_team <- renderTable({
    
    if(is.null(rvTeam$League) | is.null(rvTeam$Team)) return(NULL)
    
    
    if(input$tt_tactic == "4-4-2"){
      
      tac <- input$tt_tactic
      
    }else if(input$tt_tactic == "4-3-3"){
      
      tac <- input$tt_tactic
      
    }else if(input$tt_tactic == "3-5-2"){
      
      tac <- input$tt_tactic
      
    }else{
      return(NULL)
    }
    
    teamTeam <- best_team(bestTeam, input = tac)
    
    
    if(nrow(teamTeam) < 11){
      sendSweetAlert(session = session, type = "error", title = "This tactic is not suitable for your team!",
                     text = "Try other tactics!")
    }
    
    if(nrow(teamTeam) == 11){
      sendSweetAlert(session = session, type = "success", title = "This tactical is suitable for your team!")
    }
    
    
   teamTeam %>% 
     add_row(Name = "Average Overall:", Overall = round(mean(teamTeam$Overall), digits = 2), Position = "ALL") %>% 
     mutate(` ` = 1:length(Name),
            ` ` = if_else(` ` == max(` `), NA_integer_, ` `),
            ` ` = as.character(` `),
            ` ` = if_else(is.na(` `), "", ` `),
            Jersey.Number = as.character(Jersey.Number),
            Jersey.Number = if_else(is.na(Jersey.Number), "", Jersey.Number),
            Club = as.character(Club),
            Club = if_else(is.na(Club), "", Club)) %>% 
     select(` `, everything()) %>% 
     rename_all(funs(gsub("[[:punct:]]", " ", .)))



  })
  
  
  # Best Team Plot
  output$tt_best_team_plot <- renderPlot({
    
    teamTeam <- best_team(bestTeam, input = input$tt_tactic)
    
    img <- image_read("database/pitch.png")
      
    formation442 <- data.frame(Position = as.factor(c("GK","CB", "CB", "CB", "RM", "CM","CM", "CM", "LM", "ST", "ST")), 
                               X = c(5, 17, 17, 17, 17, 35, 35, 35, 35, 60, 60), 
                               Y = c(50 ,16 ,41, 58, 83, 16, 41, 58, 83, 33, 66))
    
    formation433 <- data.frame(Position = as.factor(c("GK", "LB", "CB","CB", "RB", "RM", "CM", "LM", "LW", "ST", "RW")), 
                               X = c(5, 17, 17, 17, 17, 35, 35, 35, 55, 60, 55), 
                               Y = c(50 ,16 ,41, 58, 83, 25, 50, 75, 25, 50, 75))
    
    formation352 <- data.frame(Position = as.factor(c("GK","CB", "CB", "CB", "RM", "CM","CM", "CM", "LM", "ST", "ST")), 
                               X = c(5, 17, 17, 17, 35, 35, 35, 35, 35, 60, 60), 
                               Y = c(50 ,25 ,50, 75, 16, 35, 50, 65, 83, 33, 66))
    
    tp <- rv$df %>% 
      filter(League %in% rvTeam$League, Club %in% rvTeam$Team) %>% 
      select(Class, Overall) %>% 
      group_by(Class) %>% 
      summarise(Overall = round(mean(Overall, na.rm = TRUE)))
    
    if(input$tt_tactic == "4-4-2"){
      
      formation <- formation442
      
    }else if(input$tt_tactic == "4-3-3"){
      
      formation <- formation433
      
    }else if(input$tt_tactic == "3-5-2"){
      
      formation <- formation352
      
    }else{
      return(NULL)
    }
    
    if(nrow(teamTeam) < 11){

      ggplot()+
        background_image(img)+
        theme(panel.background = element_rect(fill = "forestgreen",
                                              size = 0.5, linetype = "solid"))

    }else{

      ggplot(cbind(teamTeam,
                   formation %>% select(-Position)), aes(X, Y, label = Jersey.Number))+
        background_image(img)+
        geom_point(size = 11, shape = 21, fill = "khaki", stroke = 3, color = "gold")+
        geom_text()+
        theme_void()+
        labs(caption = paste0("TOTAL TEAM POWER \n","Goal Keeper: ", tp$Overall[1], " | ", "Defender: ", tp$Overall[2], " | ",
                              "Midfielder: ", tp$Overall[3], " | ", "Forward: ", tp$Overall[4]))+
        theme(panel.background = element_rect(fill = "forestgreen",
                                              size = 0.5, linetype = "solid"),
              plot.caption = element_text(hjust=0.5, color = "seagreen", size=rel(1.2)))+
        fill_palette("jco")+
        xlim(0,70)
        
      
      }

  })
  

  
  
# 4. Stats -------------------------------------------------------------------   
  
  output$tt_stats <- renderTable({
    
    if(is.null(rvTeam$League) | is.null(rvTeam$Team)) return(NULL)
    
    dfTop <- rv$df  %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team) 

      suppressWarnings(bind_rows(
        
        headTail(dfTop %>% arrange(-Age), top = 1,bottom = 1) %>% na.omit() %>%  
          mutate(Top = if_else(Age == max(Age), "Oldest", "Youngest")) %>% select(Name, Top, Age) %>% rename(Feature = Age) %>% 
          mutate_at(vars(c(Name, Top, Feature)), funs(as.character)),
        
        headTail(dfTop %>% arrange(-Values), top = 1,bottom = 1) %>% na.omit() %>%  
          mutate(Top = if_else(Values == min(Values),"Most Worthless", "Most Valuable")) %>% 
          select(Name, Top, Value) %>% rename(Feature = Value)%>% 
          mutate_at(vars(c(Name, Top, Feature)), funs(as.character)),
        
        headTail(dfTop %>% arrange(-Overall), top = 1,bottom = 1) %>% na.omit() %>%  
          mutate(Top = if_else(Overall == max(Overall), "Best Player", "Worst Player")) %>% select(Name, Top, Overall) %>% rename(Feature = Overall) %>% 
          mutate_at(vars(c(Name, Top, Feature)), funs(as.character)),
        
        headTail(dfTop %>% arrange(-Penalties), top = 1,bottom = 1) %>% na.omit() %>% 
          mutate(Top = if_else(Penalties == max(Penalties), "Best Penalty Taker", "Worst Penalty Taker")) %>% 
          select(Name, Top, Penalties) %>% rename(Feature = Penalties) %>% 
          mutate_at(vars(c(Name, Top, Feature)), funs(as.character)),
        
        headTail(dfTop %>% arrange(-FK.Accuracy), top = 1,bottom = 1) %>% na.omit() %>% 
          mutate(Top = if_else(FK.Accuracy == max(FK.Accuracy), "Best Free Kick Taker", "Worst Free Kick Taker")) %>% 
          select(Name, Top, FK.Accuracy) %>% rename(Feature = FK.Accuracy) %>% 
          mutate_at(vars(c(Name, Top, Feature)), funs(as.character)),
        
        dfTop %>% arrange(-Vision) %>% head(1) %>% 
          rename(Feature = Vision)%>% 
          mutate(Top = "Best Game Vision: Possible Captain") %>% select(Name, Top, Feature) %>% 
          mutate_at(vars(c(Name, Top, Feature)), funs(as.character)),
        
        
      )%>% rename_all(funs(gsub("[[:punct:]]", " ", .))))
  })
  
  output$tt_heatmap <- renderD3heatmap({
    
    if(is.null(rvTeam$League) | is.null(rvTeam$Team)) return(NULL)
    
    ht <- rv$df  %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team) %>% 
      select(Name.Pos, Crossing:GK.Reflexes)
    
    rownames(ht) <- ht$Name.Pos

    ht$Name.Pos <- NULL    
    
    d3heatmap(scale(ht), colors = "RdYlBu",
              k_row = 4, # Number of groups in rows
              k_col = 2 # Number of groups in columns
    )
  })
  
  


# 5. Set Piece Goal ----------------------------------------------------------

  output$tt_spgoal1 <- renderTable({
    
    if(is.null(rvTeam$League) | is.null(rvTeam$Team)) return(NULL)
    

    rv$df %>% 
      filter(League %in% rvTeam$League, Club %in% rvTeam$Team) %>% 
      select(Name, FK.Accuracy, Curve, Shot.Power, Preferred.Foot) %>%
      arrange(-FK.Accuracy) %>% 
      head(10) %>% 
      rename_all(funs(gsub("[[:punct:]]", " ", .)))
    
  })
  
  
  output$tt_spgoal2 <- renderTable({
    
    if(is.null(rvTeam$League) | is.null(rvTeam$Team)) return(NULL)
    
    rv$df %>% 
      filter(League %in% rvTeam$League, Club %in% rvTeam$Team) %>% 
      select(Name, Penalties, Curve, Shot.Power,Preferred.Foot) %>% 
      arrange(-Penalties) %>% 
      head(10) %>% 
      rename_all(funs(gsub("[[:punct:]]", " ", .)))
    
  })
  
  
  
  
  
  

# 6. BMI ---------------------------------------------------------------------


  bmi <- rv$df %>% 
    filter(League == rvTeam$League, Club == rvTeam$Team) %>%
    mutate(BMI = round(Weight*0.453592/(Height)^2, digits = 5))%>%
    arrange(-BMI)%>%
    select(Name, Height, Weight, BMI)
  
  
  bmi2  <- rbind(
    bmi %>% head(5) %>% mutate(BMI = BMI * -1),
    bmi %>% tail(5)
  ) %>% mutate(Type = if_else(BMI < 0, "Unfit", "Fit"))
  
  output$tt_bmi <- renderTable({
    
    bmi %>% 
      headTail(top = 5, bottom = 5, digits = 5) %>% na.omit() %>% 
      mutate(` ` = c(paste0("Head ", 1:5), paste0("Tail ", 5:1)))
    
  })
  
  output$tt_bmi2 <- renderPlot({
    
    bmi2 %>% 
      ggplot(aes(reorder(Name, BMI), BMI))+
      geom_col(aes(fill = Type), show.legend = FALSE)+
      coord_flip()+
      theme_minimal()+
      theme(axis.text.x = element_blank())+
      labs(x = NULL, y = NULL)+
      scale_fill_manual(values = c("steelblue", "khaki"))
    
  })
  
  
    
})



# 3D Plot -----------------------------------------------------------------


output$tt_3d <- renderPlotly({
  
  if(is.null(rvTeam$League) | is.null(rvTeam$Team)) return(NULL)
  
  p3d <- rv$df %>% filter(League %in% rvTeam$League, Club %in% rvTeam$Team)  
  
  plot_ly(p3d, x = ~Values, y = ~Overall, z = ~Potential, color = ~Class, text = ~Name.Pos) %>% 
    add_markers() %>%
    layout(
      scene = list(xaxis = list(title = 'Value'),
                   yaxis = list(title = 'Overall'),
                   zaxis = list(title = 'Potential'))
    )
  })
  
  

