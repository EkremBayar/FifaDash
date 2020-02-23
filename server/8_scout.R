# Description -------------------------------------------------------------

# This R file is prepared for Scout page.
# There is a player database from all of leagues.
# There are the best ten players from each position classes.
# Player Stats Page contains SWOT Analysis, Player Data, Visualizations and Linear Disriminant Anaysis.
# Linear Discriminant Analysis shows us that which position the football player should play.
# There are K-NN results in the Player Clustering page. It includes visualizations and data tables.
# PCA helps to find a player and a team score. Also there are some visualizations and data tables.
# Correlation analysis used to evaluate the strength of relationship between two quantitative variables.
# Unpaired Two-Samples Wilcoxon Test is a hypothesis test. 
# Unpaired Two-Samples Wilcoxon Test investigates whether there is a significant difference between the two groups.
# Also there is a transfer page.

# 1. Player Database ------------------------------------------------------


output$scout_dt <- renderDT({
  
  
  scdt <- rv$df %>% select(-Photo, -Flag, -Club.Logo, -Name.Pos, -Country) %>% 
    arrange(-Overall) %>% rename_all(funs(gsub("[[:punct:]]", " ", .)))
  datatable(scdt,
            filter = "top",  class = "display nowrap compact",  
            options = list(scrollX = TRUE,
                           searching = TRUE))
  
})


# 2. Best Players ---------------------------------------------------------

output$GoalKeeper <- renderUI({
  
  best_overall(rv$df, class = "Goal Keeper")
  
})

output$Defender <- renderUI({
  
  best_overall(rv$df, class = "Defender")
  
})

output$Midfielder <- renderUI({
  
  best_overall(rv$df, class = "Midfielder")
  
})

output$Forward <- renderUI({
  
  best_overall(rv$df, class = "Forward")
})


# 3. Player Stats ---------------------------------------------------------

rvScout<- reactiveValues(League = NULL, Team = NULL, Player = NULL)

observeEvent(input$ts_select,{
  
  req(input$ts_league)
  req(input$ts_team)
  req(input$ts_player)
  
  rvScout$League <- input$ts_league
  rvScout$Team   <- input$ts_team
  rvScout$Player <- input$ts_player

  sdf <- rv$df %>% 
    filter(League %in% rvScout$League, Club %in% rvScout$Team, Name.Pos %in% rvScout$Player)
  
  

# 3.1. Player Photo ---------------------------------------------------------


  output$ts_player_photo <- renderUI({
    
    
    if(is.null(sdf) | nrow(sdf) == 0) return(NULL)
    
    images <- sdf %>% pull(Photo)
    names <- sdf %>% pull(Name.Pos)
    clubs <- sdf %>% pull(Club)
    logo <- sdf %>% pull(Club.Logo)
    
    # Box Profile mı yoksa widgetUserBox mı?
    
    box(
      status = "success",
      boxProfile(
        src = images,
        title = names,
        subtitle = tags$img(src = logo)))
    

  })

  
  

# 3.2. Player Stats ---------------------------------------------------------

  output$ts_player_stats <- renderDT({
    
    if(is.null(sdf) | nrow(sdf) == 0) return(NULL)
    
    dta <- sdf %>% 
      mutate(Height = paste0(Height, " cm"),
             Weight = paste0(Weight, " kg"),
             League = paste0(League, " / ", Country)) %>% 
      select(Jersey.Number, Class, Nationality, League, Age,Height,Weight, Overall, Potential, Value, Wage, 
             Contract.Valid.Until, Preferred.Foot, Weak.Foot, International.Reputation,
             Skill.Moves, Work.Rate, Crossing, Finishing, Heading.Accuracy, Short.Passing, Volleys, Dribbling,
             Curve, FK.Accuracy,Long.Passing, Ball.Control, Acceleration, Sprint.Speed, Agility, Reactions,               
             Balance, Shot.Power, Jumping, Stamina, Strength, Long.Shots, Aggression, Interceptions, Positioning,             
             Vision, Penalties, Composure, Marking, Standing.Tackle, Sliding.Tackle, GK.Diving, GK.Handling, 
             GK.Kicking, GK.Positioning, GK.Reflexes) %>% 
      rename_all(funs(gsub("[[:punct:]]", " ", .))) 
      
    
    dta2 <- data.frame(v1 = paste0(names(dta), ":"), v2 = t(dta), row.names = NULL)
    
    
    tb <- data.frame(a = dta2$v1[1:17], b = dta2$v2[1:17], c = dta2$v1[18:34], d = dta2$v2[18:34],  
                     e = dta2$v1[35:51], f = dta2$v2[35:51], row.names = NULL, check.names = FALSE)
    
    
    
    datatable(tb, rownames = FALSE, colnames = rep("", ncol(tb)),
              selection = "none", 
              options = list(
                searching = FALSE,
                paging = FALSE,
                info = FALSE,
                ordering = FALSE,
                autoWidth = TRUE)) %>% 
      formatStyle(backgroundColor = "seagreen",color = "white", target = "cell", columns = names(tb))
    
  })
  
  

  
  
  

# 3.3. Player Visualizaton --------------------------------------------------

  
  # Bar
  output$ts_player_viz <- renderPlot({
    
    if(is.null(sdf) | nrow(sdf) == 0) return(NULL)
    
    sdf %>% select(Crossing:`Sliding.Tackle`)  %>% 
      rename_all(funs(gsub("[[:punct:]]", " ", .))) %>% 
      gather(Skill, Exp, Crossing:`Sliding Tackle`) %>% 
      ggplot(aes(reorder(Skill, Exp), Exp, fill = if_else(Exp < 50, "orangered", 
                                                            if_else(Exp <60, "orange",
                                                                    if_else(Exp < 70, "goldenrod1", 
                                                                            if_else(Exp <80, "palegreen4","forestgreen"))))))+
        geom_col()+
        coord_flip()+
        theme_minimal()+
        scale_fill_viridis_d(option = "E")+
        labs(x = NULL, y = "Ability")+
        theme(legend.position='none')
    
    
  })
  
  # Dendrogram
  output$ts_dend <- renderPlot({
    
    if(is.null(sdf) | nrow(sdf) == 0) return(NULL)
    
    hh <- sdf %>% select(Crossing:`Sliding.Tackle`) %>% 
      rename_all(funs(gsub("[[:punct:]]", " ", .))) %>% 
      gather(Skill, Exp, Crossing:`Sliding Tackle`)
    row.names(hh) <- hh$Skill
    hh$Skill <- NULL
    
    hc <- hclust(dist(hh), "ave")  # hierarchical clustering
    
    # plot
    ggdendrogram(hc, rotate = TRUE)
    
  })
  

  # Player Position Power
  output$ts_playerPosPower <- renderPlot({
    
    player <- rv$df %>% 
      filter(League %in% rvScout$League, Club %in% rvScout$Team, Name.Pos %in% rvScout$Player) %>%  select(Position, LS:RB)
    
    if(player$Position == "GK"){
      
      player$Position <- NULL
      
      player <- as.data.frame(t(player))
      
      names(player) <- "Value"
      
      player %<>% 
        rownames_to_column("Pos") %>% 
        mutate(Value= 0,
               Pos = as.factor(Pos))
      
    }else{
      
      player$Position <- NULL
      
      player <- as.data.frame(t(player))
      
      names(player) <- "Value"
      
      player %<>% 
        rownames_to_column("Pos") %>% 
        mutate(Value = as.numeric(str_sub(Value, end = 2)),
               Pos = as.factor(Pos))
      
    }

      
      pos <- data.frame(Pos = as.character(c("LB","LCB","CB", "RCB","RB",
                                             "LWB", "LDM", "CDM", "RDM", "RWB",
                                             "LM", "LCM", "CM", "RCM", "RM",
                                             "LAM", "CAM", "RAM",
                                             "LW","LF","CF","RF","RW",
                                             "LS","ST","RS")), 
                        x = c(1:5,
                              1:5,
                              1:5,
                              2:4,
                              1:5,
                              2:4),
                        y = c(rep(1,5), 
                              rep(1.5,5),
                              rep(2,5),
                              rep(2.5,3),
                              rep(3,5),
                              rep(3.5,3)))
      
      player <- left_join(player, pos, by = 'Pos')
      
      ggplot(player, aes(x, y, fill = if_else(Value < 50, "orangered", 
                                              if_else(Value <60, "orange",
                                                      if_else(Value < 70, "goldenrod1", 
                                                              if_else(Value <80, "palegreen4",
                                                                      if_else(Value < 90, "forestgreen",
                                                                              if_else(Value == 0, "orangered","darkgreen"))))))))+
        geom_point(shape = 22, size = 20, show.legend = FALSE)+
        geom_text(aes(label = Pos), vjust= -0.5, color = "white", size = 4.5, fontface = "bold")+
        geom_text(aes(label = Value), vjust = 1.5, fontface = "bold", color = "white")+
        ylim(0.8, 4)+
        theme_void()+
        scale_fill_identity()+
        scale_x_continuous(limits = c(0, 6))
      
    
    
  })
  


  
  

# 3.4. Best Skill ---------------------------------------------------------


  
  
  output$ts_slist <- renderUI({
    
    if(is.null(sdf) | nrow(sdf) == 0) return(NULL)
    
    slist <- sdf %>%  select(Crossing:GK.Reflexes) %>% 
      rename_all(funs(gsub("[[:punct:]]", " ", .))) %>% 
      gather(Skill, Exp, Crossing:`GK Reflexes`) %>% arrange(-Exp) %>% head(5) 
    
    skill <- slist %>% pull(Skill)
    exp <- slist %>% pull(Exp)
    
    navPills(
      navPillsItem(
        active = TRUE,
        pillName = skill[1], 
        pillColor = "green",
        pillIcon = "fa fa-angle-up", 
        pillText = exp[1]
        ),
      navPillsItem(
        active = TRUE,
        pillName = skill[2], 
        pillColor = "green",
        pillIcon = "fa fa-angle-up", 
        pillText = exp[2]
      ),
      navPillsItem(
        active = TRUE,
        pillName = skill[3], 
        pillColor = "green",
        pillIcon = "fa fa-angle-up", 
        pillText = exp[3]
      ),
      navPillsItem(
        active = TRUE,
        pillName = skill[4], 
        pillColor = "green",
        pillIcon = "fa fa-angle-up", 
        pillText = exp[4]
      ),
      navPillsItem(
        active = TRUE,
        pillName = skill[5], 
        pillColor = "green",
        pillIcon = "fa fa-angle-up", 
        pillText = exp[5]
      )
    )
    
  })
  
  
  output$ts_slist2 <- renderUI({
    
    if(is.null(sdf) | nrow(sdf) == 0) return(NULL)
    
    slist <- sdf 
    
    if(sdf$Position == "GK"){
      
      slist <- slist %>% select(Crossing:GK.Reflexes) %>% 
        rename_all(funs(gsub("[[:punct:]]", " ", .))) %>% 
        gather(Skill, Exp, Crossing:GK.Reflexes) %>% arrange(-Exp) %>% tail(5) 
      
    }else{
      
      slist <- slist %>% select(Crossing:GK.Reflexes) %>% 
        rename_all(funs(gsub("[[:punct:]]", " ", .))) %>% 
        gather(Skill, Exp, Crossing:`GK Reflexes`) %>% arrange(-Exp) %>% tail(10)
      
    }
    
    
    skill <- slist %>% pull(Skill)
    exp <- slist %>% pull(Exp)
    
    navPills(
      navPillsItem(
        active = TRUE,
        pillName = skill[1], 
        pillColor = "red",
        pillIcon = "fa fa-angle-down", 
        pillText = exp[1]
      ),
      navPillsItem(
        active = TRUE,
        pillName = skill[2], 
        pillColor = "red",
        pillIcon = "fa fa-angle-down", 
        pillText = exp[2]
      ),
      navPillsItem(
        active = TRUE,
        pillName = skill[3], 
        pillColor = "red",
        pillIcon = "fa fa-angle-down", 
        pillText = exp[3]
      ),
      navPillsItem(
        active = TRUE,
        pillName = skill[4], 
        pillColor = "red",
        pillIcon = "fa fa-angle-down", 
        pillText = exp[4]
      ),
      navPillsItem(
        active = TRUE,
        pillName = skill[5], 
        pillColor = "red",
        pillIcon = "fa fa-angle-down", 
        pillText = exp[5]
      )
    )
    
  })
  
  
  
  
  
  
  

# 3.5. Linear Discriminant Analysis ---------------------------------------

  # Data Preparing for LDA 
  ldf <- rv$df %>% 
    select(Name.Pos, Position, Crossing:GK.Reflexes) %>%  
    mutate(Name.Pos = paste0(1:nrow(rv$df), "-", Name.Pos),
           Position = as.factor(as.character(Position))) %>% 
    column_to_rownames("Name.Pos")
  
  # Model
  model_lda <- MASS::lda(Position~., data = ldf)
  
  # Make predictions
  predictions <- model_lda %>% predict(ldf)
  
  # Tahminler ile Gerçek Matris
  table(predictions$class, ldf$Position)
  
  # All posibilities
  ldadf2 <- as.data.frame(predictions$posterior) %>% 
    mutate(name = row.names(predictions$posterior)) %>% 
    cbind(rv$df %>% select(League, Club)) %>% 
    gather(Skill, Exp, CAM:ST, -name) %>% 
    arrange(name, -Exp) %>% 
    separate(name, c("number", "Name.Pos"), sep = "-") %>% 
    select(-number) %>% 
    filter(League == rvScout$League, Club == rvScout$Team, Name.Pos == rvScout$Player) 
  
  output$ldaplot <- renderPlot({
    
    ggplot(ldadf2, aes(reorder(Skill,Exp), Exp, fill = Exp, label = paste0(round(Exp, digits = 4)*100, "%")))+
      geom_col()+
      geom_text(hjust = 0)+
      coord_flip()+
      theme_minimal()+
      theme(legend.position = "bottom")+
      scale_fill_gradient(low = "khaki", high = "seagreen")+
      labs(x = NULL, y = "Probability", fill = "Probability")
    
  })
  
  
})




# 4. Player Clustering ----------------------------------------------------

rvCluster <- reactiveValues(GKcluster = NULL, DFcluster = NULL, MDcluster = NULL, FWcluster = NULL)

observe({
  
  rvCluster$GKcluster <- km(rv$df, position = "Goal Keeper")
  rvCluster$DFcluster <- km(rv$df, position = "Defender")
  rvCluster$MDcluster <- km(rv$df, position = "Midfielder")
  rvCluster$FWcluster <- km(rv$df, position = "Forward")
  
  
})

observe({
  
  req(input$cl_cluster)
  
  output$cluster <- renderPlot({
    
    if(input$cl_cluster == "Goal Keeper"){
      
      cl <- rvCluster$GKcluster
    }else if(input$cl_cluster == "Defender"){
      cl <- rvCluster$DFcluster
    }else if(input$cl_cluster == "Midfielder"){
      cl <- rvCluster$MDcluster
    }else{
      cl <- rvCluster$FWcluster
    }
    
    grid.arrange(
      cl$clusters$p1,
      cl$clusters$p2,
      cl$clusters$p3,
      cl$clusters$p4)
    
  })
  
  output$opplot <- renderPlot({
    
    if(input$cl_cluster == "Goal Keeper"){
      
      cl <- rvCluster$GKcluster
    }else if(input$cl_cluster == "Defender"){
      cl <- rvCluster$DFcluster
    }else if(input$cl_cluster == "Midfielder"){
      cl <- rvCluster$MDcluster
    }else{
      cl <- rvCluster$FWcluster
    }
    
    grid.arrange(cl$clusters_opt$op1, cl$clusters_opt$op2)
  })
  
  output$clusterMean <- renderDT({
    
    if(input$cl_cluster == "Goal Keeper"){
      
      cl <- rvCluster$GKcluster
    }else if(input$cl_cluster == "Defender"){
      cl <- rvCluster$DFcluster
    }else if(input$cl_cluster == "Midfielder"){
      cl <- rvCluster$MDcluster
    }else{
      cl <- rvCluster$FWcluster
    }
    
    clustermean <- cl$data %>%
      group_by(Cluster) %>% 
      summarise_if(is.numeric, mean, na.rm = TRUE) %>%
      mutate_if(is.numeric, function(x){round(x, digits = 2)}) %>% 
      mutate(Value = if_else(Values < 1000000, paste0("€", round(Values/1000), "K"), paste0("€", round(Values/1000000), "M"))) %>% 
      mutate(Wage = paste0("€", round(Wages/1000), "K")) %>% 
      select(Cluster, Value, Wage, everything(),-Values, -Wages, -Special,	-International.Reputation,	-Weak.Foot,	-Skill.Moves,	-Jersey.Number,	-Height,	-Weight)
    
    datatable(clustermean %>% rename_all(funs(gsub("[[:punct:]]", " ", .))),
              rownames = FALSE,
              class = 'cell-border stripe',
              selection = "none",
              filter = "none",
              options = list(dom='lfrtip',
                             scrollX = TRUE,
                             searching = FALSE,
                             ordering= FALSE,
                             lengthChange = 11,
                             paging = FALSE,
                             info = FALSE))
    
  })
  
  output$clusterdt <- renderDT({
    
    if(input$cl_cluster == "Goal Keeper"){
      
      cl <- rvCluster$GKcluster
    }else if(input$cl_cluster == "Defender"){
      cl <- rvCluster$DFcluster
    }else if(input$cl_cluster == "Midfielder"){
      cl <- rvCluster$MDcluster
    }else{
      cl <- rvCluster$FWcluster
    }
    
    datatable(cl$data %>% select(-Values, -Wages, -Name.Pos, -Special) %>% rename_all(funs(gsub("[[:punct:]]", " ", .))),
              filter = "top",  class = "display nowrap compact",  
              options = list(scrollX = TRUE,
                             searching = TRUE))
    
  })
  
  
  
  
})



# 5. Principle Component Analysis -----------------------------------------

rvPCA <- reactiveValues(pca = NULL)

observe({
  
  pca_gk <- pca_fifa(rv$df, position = "Goal Keeper")
  pca_df <- pca_fifa(rv$df, position = "Defender")
  pca_md <- pca_fifa(rv$df, position = "Midfielder")
  pca_fw <- pca_fifa(rv$df, position = "Forward")
  
  rvPCA$pca <- list("GK" = pca_gk, "DF" = pca_df, "MD" = pca_md, "FW" = pca_fw)
  
})

observe({
  
  req(input$pca_class)
  
  if(input$pca_class == "Goal Keeper"){
    
    dfpca <- rvPCA$pca$GK
    
  }else if(input$pca_class == "Defender"){
    
    dfpca <- rvPCA$pca$DF
    
  }else if(input$pca_class == "Midfielder"){
    
    dfpca <- rvPCA$pca$MD
    
  }else if(input$pca_class == "Forward"){
    
    dfpca <- rvPCA$pca$FW
    
  }else{
    return(NULL)
  }
  
  
  output$pca_players <- renderDataTable({
    
    datatable(dfpca$players %>% select(-Class),
              class = 'cell-border stripe',
              selection = "none",
              filter = "none",
              options = list(dom='lfrtip',
                             searching = FALSE,
                             ordering= FALSE,
                             lengthChange = FALSE,
                             info = FALSE))
    
  })
  
  
  # PCA Club
  output$pca_clubs <- renderDataTable({
    datatable(dfpca$clubs,
              class = 'cell-border stripe',
              selection = "none",
              filter = "none",
              options = list(dom='lfrtip',
                             searching = FALSE,
                             ordering= FALSE,
                             lengthChange = FALSE,
                             info = FALSE))
  })
  
  
  output$pca_players_plot <- renderPlot({
    ggplot(dfpca$players)+
      geom_text(aes(Overall, `PCA Score`,alpha = Overall, label = Name, 
                    color = if_else(
                      Overall >= 85 & `PCA Score` >= 7.5, "red", 
                      if_else(
                        `PCA Score` >= 6.25 & Overall >= 80, "blue", 
                        if_else(
                          `PCA Score` >= 5 & Overall >= 80, "orange", 
                          if_else(Overall >= 70 & `PCA Score` >= 5, "purple",
                                  if_else(Overall >= 75, "green",
                                          if_else(
                                            Overall >=65 & `PCA Score` >=2.5, "yellow", "gray"))))))
      ), show.legend = FALSE)+
      scale_color_manual(values = c("slateblue4", "darkslategray", "seagreen4", 
                                    "dodgerblue4", "steelblue", "royalblue4", "darkslategray4"))+
      theme_minimal()+
      labs(color = "Position", y = "PCA Score", title = "Principle Component Analysis")
  })
  
  
  output$pca_players_barplot <- renderPlot({
    
    dfpca$players %>% group_by(Class) %>% top_n(n = 15, wt = `PCA Score`) %>% 
      ggplot(aes(reorder(Name, `PCA Score`), `PCA Score`, label = paste0("PCA Score: ", `PCA Score`)))+
      geom_col(fill = "darkslategray4")+
      geom_text_repel(ylim = 0, segment.color = "white", color = "gold")+
      coord_flip()+
      theme_minimal()+
      theme(legend.position = "bottom", axis.text.x = element_blank())+
      labs(title = "PCA Scores: Top 15", x = NULL, y = NULL)
  })
  
  output$pca_players_barplot2 <- renderPlot({
    
    dfpca$players %>% group_by(Class) %>% top_n(n = 15, wt = Overall) %>% 
      ggplot(aes(reorder(Name, Overall), Overall, label = paste0("Overall: ", Overall)))+
      geom_col(fill = "darkslategray4")+
      geom_text_repel(ylim = 0, segment.color = "white", color = "gold")+
      coord_flip()+
      theme_minimal()+
      theme(legend.position = "bottom", axis.text.x = element_blank())+
      labs(title = "Overall: Top 15", x = NULL, y = NULL)
  })
  
  
})

# 6. Correlation ----------------------------------------------------------

rvCor<- reactiveValues(X = NULL, Y = NULL, League = NULL, Class = NULL)

observeEvent(input$cr_select,{
  
  req(input$cr_x)
  req(input$cr_y)
  req(input$cr_league)
  req(input$cr_class)
  req(!is.null(input$cr_x))
  
  rvCor$X <- as.character(input$cr_x)
  rvCor$Y <- as.character(input$cr_y)
  rvCor$League <- input$cr_league
  rvCor$Class <- input$cr_class
  
  corp <- rv$df %>% 
    filter(League == rvCor$League, Class == rvCor$Class) %>% 
    mutate_if(is.integer, as.numeric)
  
  xt1 <- corp %>% filter(Preferred.Foot == "Left") %>% select(!!rlang::parse_expr(rvCor$X)) %>% pull()
  xt2 <- corp %>% filter(Preferred.Foot == "Right") %>% select(!!rlang::parse_expr(rvCor$X)) %>% pull()
  yt1 <- corp %>% filter(Preferred.Foot == "Left") %>% select(!!rlang::parse_expr(rvCor$Y)) %>% pull()
  yt2 <- corp %>% filter(Preferred.Foot == "Right") %>% select(!!rlang::parse_expr(rvCor$Y)) %>% pull()
  xht <- wilcox.test(xt1, xt2, alternative = "two.sided")
  yht <- wilcox.test(yt1, yt2, alternative = "two.sided")
  
  lc <- data.frame(Method = c("Pearson", "Spearman", "Kendall"),
                   Cor = c(
                     cor(corp %>% pull(!!rlang::parse_expr(rvCor$X)), corp %>% pull(!!rlang::parse_expr(rvCor$Y)), method = "pearson"),
                     cor(corp %>% pull(!!rlang::parse_expr(rvCor$X)), corp %>% pull(!!rlang::parse_expr(rvCor$Y)), method = "spearman"),
                     cor(corp %>% pull(!!rlang::parse_expr(rvCor$X)), corp %>% pull(!!rlang::parse_expr(rvCor$Y)), method = "kendall")
                   ))
  
  output$cr_corp <- renderPlot({
    
    corp %>%
      ggplot(aes(!!rlang::parse_expr(rvCor$X), !!rlang::parse_expr(rvCor$Y), color = Preferred.Foot, label = Name))+
      geom_text() +
      geom_jitter(alpha = 0.2, size = 2.5, width = 0.3, height = 0.3)+
      geom_smooth(method = "lm", color = "gray40", lty = 2, se = FALSE, size = 0.6)+
      theme_minimal()+
      theme(legend.position = "bottom",
            plot.title = element_text(color = "royalblue"),
            plot.subtitle = element_text(color = "darkslategray4"))+
      scale_color_manual(values = c("orangered","steelblue"))+
      labs(color = "Preferred Foot", x = as.character(rvCor$X), y = as.character(rvCor$Y),
           title = rvCor$League,
           subtitle = paste0("Pearson Correlation Coefficent: ", round(lc$Cor[1], digits = 4),
                             "\n",
                             "Spearman Correlation Coefficient: ", round(lc$Cor[2], digits = 4),
                             "\n",
                             "Kendall Correlation Coefficient: ", round(lc$Cor[3], digits = 4)))
  })
  
  
  
  
  
  output$cr_htest <- renderPlot({
    
    if(xht$p.value < 0.05 | yht$p.value < 0.05){
      
      messageHTest <- "The p-value of the test is less than 0.05."
      messageSTest <- "There is significantly difference between right foot and left foot values."
      
    }else{
      messageHTest <- "The p-value of the test is not less than 0.05."
      messageSTest <- "There is no significantly difference between right foot and left foot values."
    }
    
    
    grid.arrange(ncol = 2,
                 
                 ggplot(corp, aes(x = Preferred.Foot, y = !!rlang::parse_expr(rvCor$X), fill = Preferred.Foot))+
                   geom_boxplot(show.legend = FALSE)+
                   theme_minimal()+
                   scale_fill_manual(values = c("orangered", "steelblue"))+
                   ylim(30,100)+
                   labs(x = "Preferred Foot",
                        y = rvCor$X,
                        title = messageHTest,
                        subtitle = messageSTest,
                        caption = paste0("p Value: ", round(xht$p.value, digits = 3))),
                 
                 ggplot(corp, aes(x = Preferred.Foot, y = !!rlang::parse_expr(rvCor$Y), fill = Preferred.Foot))+
                   geom_boxplot(show.legend = FALSE)+
                   theme_minimal()+
                   scale_fill_manual(values = c("orangered", "steelblue"))+
                   ylim(30,100)+
                   labs(x = "Preferred Foot",
                        y = rvCor$Y,
                        title = messageHTest,
                        subtitle = messageSTest,
                        caption = paste0("p Value: ", round(yht$p.value, digits = 3)))
    )
    
    
  })
  
  
})


# 7. Player Transfer ------------------------------------------------------

rv <- reactiveValues(budget = NULL, transfer_df = NULL)

observe({
  
  rv$transfer_df <- bind_rows(rvCluster$GKcluster$data, rvCluster$DFcluster$data, 
                              rvCluster$MDcluster$data, rvCluster$FWcluster$data)
  
})


# 7.1. Searching Player ---------------------------------------------------


output$transfer <- renderDT({

  temp <- rv$transfer_df
  
  temp <- temp %>%
    mutate(Cluster_num = str_sub(temp$Cluster, start = 2)) %>%
    filter(Values <= max(input$s_value), Values >= min(input$s_value),
           Age <= max(input$s_age), Age >= min(input$s_age),
           Overall <= max(input$s_overall), Overall >= min(input$s_overall),
           Class %in% input$s_class,
           Cluster_num %in% input$s_cluster,
           League %in% input$s_league,
           Preferred.Foot %in% input$s_prfoot,
           International.Reputation %in% input$s_inrep) %>%
    arrange(-Values, -Overall, -Age) %>%
    select(-Name.Pos, -Values, -Wages, -Cluster_num, -LS:-RB, -Special)


  datatable(temp %>% 
              rename_all(funs(gsub("[[:punct:]]", " ", .))),
            options = list(scrollX = TRUE,
                           searching = TRUE))
})



# 7.2. Transfer Player ----------------------------------------------------

observeEvent(input$trans,{
  
  enable("sell")
  
  
  
  if(!input$text %in% (rv$df %>% pull(Name) %>% as.character)){
    
    sendSweetAlert(session = session, type = "warning", title = "Please enter a valid player name!", 
                   closeOnClickOutside = FALSE)
  }
  
  if(input$text %in% (rv$df %>% pull(Name) %>% as.character)){
    
    sendSweetAlert(session = session, type = "success",title = paste0(input$text," joined your team!"), 
                   closeOnClickOutside = FALSE, btn_labels = "Ok")
  }
  
  
  data <- read.csv("database/transfer.csv", stringsAsFactors = FALSE, encoding = "UTF-8")[-1] %>% mutate_all(as.character)
  
  
  temp <- rv$transfer_df

  if(nrow(data) == 0){

    team <- temp %>% mutate_all(as.character) %>%
      filter(Name %in% input$text) %>% select(Name, Position, Age, Overall, Club, Value, Values, Nationality, Preferred.Foot, International.Reputation)


  }else if(nrow(data) > 0){
    
    if((data %>% filter(Name == input$text) %>% pull(Name) %>% length) > 0){
      sendSweetAlert(session = session, type = "warning", title = paste0(input$text," is already on the team!"),
                     closeOnClickOutside = FALSE)
      return(NULL)

    }

    team <- data %>% select(Name,Position, Age, Overall, Club, Value, Values, Nationality, Preferred.Foot, International.Reputation) %>%  mutate_all(as.character)

    database <- temp %>% mutate_all(as.character) %>%
      filter(!Name %in% (team %>% mutate_all(as.character) %>% pull(Name))) %>%
      select(Name, Position, Age, Overall, Club, Value, Values, Nationality, Preferred.Foot, International.Reputation)

    team %<>% bind_rows(database %>% mutate_all(as.character) %>% filter(Name %in% input$text) %>% head(1))%>% 
      select(Name, Position, Age, Overall, Club, Value, Values, Nationality, Preferred.Foot, International.Reputation)
    
    
  }else{
    return(NULL)
  }

  
 
  
  rv$budget <- sum(as.numeric(team[,"Values"]))

  write.csv(team, "database/transfer.csv")


  output$Wasted <- renderInfoBox({

    bud <- 200000000 - rv$budget
    
    if(bud < 0){sendSweetAlert(session = session, type = "warning",title = "No Budget!", closeOnClickOutside = FALSE, btn_labels = "Ok")}
    
    infoBox(
      "Remaining", paste0("€", suppressWarnings(comma(bud,big.mark = "."))), icon = icon("coins"),
      color = "purple"
    )

  })


  output$transferPlayer <- renderDT({

    datatable(team %>% select(-Values) %>% rename_all(funs(gsub("[[:punct:]]", " ", .))),
              options = list(dom='lfrtip',
                             paging = FALSE,
                             filter = FALSE,
                             searching = FALSE,
                             info = FALSE))

  })
  
  



})




observeEvent(input$sell,{
  
  data <- read.csv("database/transfer.csv", stringsAsFactors = FALSE)[-1] %>% mutate_all(as.character)
  
  
  if(!input$text %in% (data %>% pull(Name) %>% as.character)){
    sendSweetAlert(session = session, type = "warning", title = "You don't have this player!", 
                   closeOnClickOutside = FALSE)
    }
  
  
  team <- data %>% filter(!Name %in% input$text)
  
  rv$budget <- 200000000 - sum(as.numeric(team[,"Values"]))
  
  write.csv(team, "database/transfer.csv")
  
  
  output$Wasted <- renderInfoBox({
    
    infoBox(
      "Remaining Budget", paste0("€", suppressWarnings(comma(rv$budget,big.mark = "."))), icon = icon("coins"),
      color = "purple"
    )
    
  })
  
  
  
  output$transferPlayer <- renderDT({
    
    datatable(team %>% select(-Values) %>% rename_all(funs(gsub("[[:punct:]]", " ", .))),
              options = list(dom='lfrtip',
                             paging = FALSE,
                             filter = FALSE,
                             searching = FALSE,
                             info = FALSE))
    
  })
  
})











 

