# Description -------------------------------------------------------------

# There are many functions written by me in this R file.
# "km" function works for K-NN algorithm, Visualization and Reporting. 
# "best_team" function finds the best players in terms of tactics.
# "value_wage" function visualize boxplots of player values and wages.
# "best_overall" function finds the best players of each class.
# "pca_fifa" function works for PCA algorithm, Visualization and Reporting. 
# "similarity" function finds similar players.
# "vbox_players" function is for value boxes.
# "imgplayer" function produce player images.
# "facetReactiveBar" and "facetReactiveLine" visualize bar and line plots.

# 1. Player Cluster -------------------------------------------------------

km <- function(df, position = c("Goal Keeper", "Defender", "Midfielder", "Forward")){
  
  res <- NULL
  
  if(missing("df") | missing("position")) return(res)
  if(is.null(df) | is.null(position)) return(res)
  
  df %<>% filter(Class == position) 
  
  x <- df
  
  x$ID <- 1:nrow(x)
  
  row.names(x) <- paste(x$ID, x$Name, sep = "-")
  x$Name <- NULL
  x$ID <- NULL
  
  x <- x %>% select_if(is.numeric) %>% select(-Jersey.Number, -Skill.Moves, -Special) %>% na.omit()
  
  x <- apply(x, 2, scale)
  
  x <- as.data.frame(x)
  
  k2 <- kmeans(x, center = 2, nstart = 25)   
  k3 <- kmeans(x, centers = 3, nstart = 25)
  k4 <- kmeans(x, centers = 4, nstart = 25)
  k5 <- kmeans(x, centers = 5, nstart = 25)
  
  p1 <- fviz_cluster(k2, geom = "point", data = x, ggtheme = theme_minimal())+
    ggtitle("k = 2")
  p2 <- fviz_cluster(k3, geom = "point", data = x, ggtheme = theme_minimal())+
    ggtitle("k = 3")
  p3 <- fviz_cluster(k4, geom = "point", data = x, ggtheme = theme_minimal())+
    ggtitle("k = 4")
  p4 <- fviz_cluster(k5, geom = "point", data = x, ggtheme = theme_minimal())+
    ggtitle("k = 5")
  
  set.seed(123)
  op1 <- fviz_nbclust(x, kmeans, method = "wss")
  op2 <- fviz_nbclust(x, kmeans, method = "silhouette")
  
  
  df <- na.omit(df) 
  
  df %<>% mutate(Cluster = as.character(paste0(str_sub(df$Class, end = 1),k4$cluster)))
  
  df <- df %>% arrange(-Overall) %>% 
    select(Cluster, Name, Club, Position, Class, League, Nationality, everything(), -Photo, -Flag, -Club.Logo, -Country)
  
  
  res <- list(clusters = list(p1 = p1, p2 = p2, p3 = p3, p4 = p4),
                clusters_opt = list(op1 = op1, op2 = op2),
                data = df)
  
  return(res)
}



# 2. Best Team ------------------------------------------------------------

best_team <- function(df, input){
  
  team <- NULL
  
  if(missing("df") | missing("input")) return(res)
  if(is.null(df) | is.null(input)) return(res)
  
  team <- tibble()
  team_copy <- df %>% select(Jersey.Number, Name, Overall, Position, Club) %>% arrange(-Overall) 
  
  tac442 <- c("GK","RB", "CB", "CB", "LB", "RM", "CM", "CM", "LM", "ST", "ST")
  tac352 <- c("GK","CB", "CB", "CB", "RM", "CM", "CM", "CM", "LM", "ST", "ST")
  tac433 <- c("GK","RB", "CB", "CB", "LB", "CM", "CDM", "CM", "LW", "RW", "ST")
  
  tactic <- if(input == "4-4-2"){
    tac442
  }else if(input == "3-5-2"){
    tac352
  }else{
    tac433
  }
  
  for (i in tactic) {
    
    team %<>%  bind_rows(team_copy %>% filter(Position %in% i) %>% head(1))
    team_copy %<>% filter(!Name %in% (team %>% pull(Name)))
    
  }
  
  return(team)
  
}





# 3. Value & Wage Leagues Boxplot Visualization ---------------------------

value_wage <- function(df, variable = c("Value", "Wage"), x = c("League", "Club")){
  
  res <- NULL
  
  if(missing("df") | missing("variable") | missing("x")) return(res)
  if(is.null(df) | is.null(variable) | is.null(x)) return(res)
  
  if(variable == "Value"){
    
    variable <- "Values"
    
  }else if(variable == "Wage"){
    
    variable <- "Wages"
    
  }else{
    
    return(NULL)
    
  }
  
  if(x == "League"){
    
    xvar <- "League"
    
  }else if(x == "Club"){
    
    xvar <- "Club"
    
  }else{
    
    return(NULL)
    
  }
  
  res <- df %>%
    ggplot(aes(reorder(!!rlang::parse_expr(xvar), !!rlang::parse_expr(variable)), !!rlang::parse_expr(variable), fill = !!rlang::parse_expr(xvar)))+
    geom_boxplot(show.legend = FALSE)+
    coord_flip()+
    theme_minimal()+
    labs(x = NULL, y = str_sub(variable, start = 1, end = (str_length(variable)-1) ))+
    scale_fill_viridis_d(begin = 0.2,end = 0.6)+
    theme(axis.line.y = element_line(colour = "darkslategray"),
          axis.ticks.x = element_line(colour = "darkslategray"))
  
  return(res)
}

  


# 4. Best Overall Class ------------------------------------------------------

best_overall <- function(df, class = c("Goal Keeper", "Defender", "Midfielder", "Forward")){
  
  if(missing("df") | missing("class")) return(NULL)
  if(is.null(df) | is.null(class)) return(NULL)
  
  class <- df %>%
    arrange(-Overall) %>% 
    filter(Class %in% class) %>% 
    head(10)
  
  
  images <-  class %>% pull(Photo)
  names <- class %>% pull(Name.Pos)
  clubs <- class %>% pull(Club)
  logo <- class %>% pull(Club.Logo)
  overall <- class %>% pull(Overall)
  
  suppressWarnings(widgetUserBox(
    title = tags$p(names[1], style = "font-size: 60%;"),
    color = "purple",
    subtitle = div(tags$p(clubs[1], style = "font-size: 80%;"), tags$img(src = logo[1]), overall[1]),
    type = 2,
    width = 3,
    src = images[1],
    collapsible = FALSE,
    closable = FALSE,
    
    footer = productList(
      productListItem(
        src = images[2], 
        productTitle = names[2], 
        productPrice = overall[2], 
        priceColor = "success",
        tags$img(src = logo[2]),clubs[2]
      ),
      productListItem(
        src = images[3], 
        productTitle = names[3], 
        productPrice = overall[3], 
        priceColor = "success",
        tags$img(src = logo[3]),clubs[3]
      ),
      productListItem(
        src = images[4], 
        productTitle = names[4], 
        productPrice = overall[4], 
        priceColor = "success",
        tags$img(src = logo[4]), clubs[4]
      ),
      productListItem(
        src = images[5], 
        productTitle = names[5], 
        productPrice = overall[5], 
        priceColor = "success",
        tags$img(src = logo[5]), clubs[5]
      ),
      productListItem(
        src = images[6], 
        productTitle = names[6], 
        productPrice = overall[6], 
        priceColor = "success",
        tags$img(src = logo[6]), clubs[6]
      ),
      productListItem(
        src = images[7], 
        productTitle = names[7], 
        productPrice = overall[7], 
        priceColor = "success",
        tags$img(src = logo[7]),clubs[7]
      ),
      productListItem(
        src = images[8], 
        productTitle = names[8], 
        productPrice = overall[8], 
        priceColor = "success",
        tags$img(src = logo[8]), clubs[8]
      ),
      productListItem(
        src = images[9], 
        productTitle = names[9], 
        productPrice = overall[9], 
        priceColor = "success",
        tags$img(src = logo[9]), clubs[9]
      ),
      productListItem(
        src = images[10], 
        productTitle = names[10], 
        productPrice = overall[10], 
        priceColor = "success",
        tags$img(src = logo[10]), clubs[10]
      )
      
    )
  )
  )
  
}







# 5. PCA ------------------------------------------------------------------

pca_fifa <- function(df, position = c("Goal Keeper", "Defender", "Midfielder", "Forward")){
  
  res <- NULL
  
  if(missing("df") | missing("position")) return(res)
  if(is.null(df) | is.null(position)) return(res)
  
  if(position == "Goal Keeper"){
    
    pca_df <- df %>% 
      filter(Position == "GK") %>% 
      select(Name, Position, Club, Class, Age, Overall, Potential, Values, Wages, Agility, 
             Reactions, Jumping, Strength, Vision, Composure, GK.Diving:GK.Reflexes)
    
    pca_df2 <- pca_df %>% 
      select(-Club, -Class, -Position) %>% 
      mutate(Name = paste0(1:nrow(pca_df), "-", Name)) %>%
      mutate_at(vars(Age:GK.Reflexes), funs(scale)) %>% 
      column_to_rownames("Name")
    
  }else{
    
    pca_df <- df %>% 
      filter(Position != "GK", Class == position) %>% 
      select(Name, Position, Club, Class, Age, Overall, Potential, Values, Wages, Crossing:Sliding.Tackle) 
    
    pca_df2 <- pca_df %>% 
      select(-Club, -Class, -Position, -Overall) %>% 
      mutate(Name = paste0(1:nrow(pca_df), "-", Name)) %>%
      mutate_at(vars(Age:Sliding.Tackle), funs(scale)) %>% 
      column_to_rownames("Name")
    
  }

  pca_df2 <- prcomp(pca_df2, scale = FALSE)
  
  pca_df2$rotation <- -pca_df2$rotation
  pca_df2$x <- -pca_df2$x
  
  
  # Kümülatif Açıklanabilir Varyans Oranı
  pl_avo <- pca_df2$sdev^2 / sum(pca_df2$sdev^2)
  
  # Player PCA Score
  
  pca_df$`PCA Score` <- rowSums(pca_df2$x[,1:6] * t(matrix(pl_avo[1:6], 6, nrow(pca_df))))

  if(position == "Goal Keeper"){
    number <- 3.969
    mul <- -1
  }else if(position == "Defender"){
    number <- 4.532
    mul <- 1
  }else if(position == "Midfielder"){
    number <- 3.921
    mul <- 1
  }else if(position == "Forward"){
    number <- 3.94
    mul <- -1
  }else{
    return(NULL)
  }

  pca_df3 <- pca_df %>% 
    mutate(`PCA Score` = mul*`PCA Score`) %>% 
    select(Name, Overall, Age, Position, Class, Club, `PCA Score`) %>% 
    arrange(-`PCA Score`) %>% 
    mutate(`PCA Score` = round(`PCA Score` + number, digits = 3)) # Buraya topla
  

  # Club PCA Score
  pca_club <- pca_df3 %>% 
    group_by(Club) %>% 
    summarise(`PCA Score (Avg)` = round(mean(`PCA Score`), digits = 2)) %>% 
    arrange(-`PCA Score (Avg)`)
  
  res <- list("players" = pca_df3, 
              "clubs" = pca_club)
  
  
  return(res)
  
  
  
}

# 6. Similarity -----------------------------------------------------------

similarity <- function(df, player, selectLeague, fill_variable, fill_strip,
                       input = c("Eucledian", "Maximum", "Manhattan", "Canberra", "Binary", 
                                 "Minkowski", "Pearson", "Spearman", "Kendall")
                       ){
  
  res <- NULL
    
  if(missing("df") | missing("input") | missing("player") | missing("fill_variable") | missing("selectLeague") | missing("fill_strip"))  return(res)
  if(is.null(df) | is.null(input) | is.null(player) | is.null(fill_variable) | is.null(selectLeague) | is.null(fill_strip)) return(res)
  if(length(selectLeague) < 1) return(res)

  if(input == "Euclidean"){
    distance_method <- "euclidean"
    }else if(input == "Maximum"){
      distance_method <- "maximum"
    }else if(input == "Manhattan"){
      distance_method <- "manhattan"
    }else if(input == "Canberra"){
      distance_method <- "canberra"
    }else if(input == "Minkowski"){
      distance_method <- "minkowski"
    }else if(input == "Pearson"){
      distance_method <- "pearson"
    }else if(input == "Spearman"){
      distance_method <- "spearman"
    }else if(input == "Kendall"){
      distance_method <- "kendall"
    }else{
      return(NULL)
    }
  
  plyr <- df %>% filter(Name.Pos == player)
  
  smdf <- df %>% filter(League %in% selectLeague, !Name.Pos %in% plyr$Name.Pos) %>% rbind(plyr)
  
  rwname <- paste0(smdf$Name.Pos, " (", smdf$Club, ")")
  
  smdf <- smdf %>% select_if(is.numeric) %>% select(-Special, -Jersey.Number)
  
  smdf <- apply(smdf, 2, scale)
  rownames(smdf) <- rwname
  
  smdf <- get_dist(smdf, method = distance_method)
  smdf <- fviz_dist(smdf)
  
  smdf <- smdf$data %>%
    mutate(Var1 = str_sub(Var1, start = 1, end = str_length(Var1)-1),
           Var2 = str_sub(Var2, start = 1, end = str_length(Var2)-1)) %>% 
    arrange(value) %>% 
    filter(value > 0, Var1 == rwname[length(rwname)]) %>% 
    head(30)
  
  res <- ggplot(smdf, aes(fct_reorder(Var2, desc(value)), value))+
    geom_col(fill = fill_variable)+
    coord_flip()+
    labs(y = "Distance", x = NULL)+
    theme_minimal()+
    facet_wrap(~paste0("Similar Players Like ", player))+
    theme(strip.background =element_rect(fill=fill_strip,color = "black"),
          strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic"))
    
    
  return(res)
  
}


# 7. Value Box for Players Page -------------------------------------------

vbox_players <- function(df, variable = c('Age', 'Overall', 'Nationality', 'Value', 'Contract.Valid.Until'), 
                         color = c("blue", "green")){
  
  res <- NULL
  
  if(missing("df") | missing("variable") | missing("color")) return(res)
  if(is.null(df) | is.null(variable) | is.null(color)) return(res)
  
  if(variable == 'Age'){
    icon_vb <- "street-view"
  }else if(variable == 'Overall'){
    icon_vb <- "battery-three-quarters"
  }else if(variable == 'Nationality'){
    icon_vb <- "flag"
  }else if(variable == 'Value'){
    icon_vb <- "wallet"
  }else if(variable == 'Contract.Valid.Until'){
    df <- df %>% rename(Contract = Contract.Valid.Until)
    variable <- "Contract"
    icon_vb <- "file-signature"
  }else{
    return(NULL)
    }
  
  res <- valueBox(
    color = color,
    value = tags$p(df %>% pull(!!rlang::parse_expr(variable)), style = "font-size: 80%;"),
    subtitle = variable,
    icon = icon(icon_vb)
  )
  
  return(res)
  
}

# 8. Players Image --------------------------------------------------------


imgplayer <- function(df){
  
  images <- NULL
  
  if(missing("df")) return(images)
  if(is.null(df)) return(images)
  
  images <- df %>% pull(Photo)
  images <- tags$img(src= images, width = 70, height = 70)

}

# 9. Facet Wrap Reactive --------------------------------------------------

facetReactiveBar <- function(df, fill_variable, fill_strip){
  
  res <- NULL
  
  if(missing("df") | missing("fill_variable") | missing("fill_strip")) return(res)
  if(is.null(df) | is.null(fill_variable) | is.null("fill_strip")) return(res)
  

  res <- df %>% 
    select(Name.Pos, Crossing:Sliding.Tackle) %>% 
    rename_all(funs(gsub("[[:punct:]]", " ", .))) %>% 
    gather(Skill, Exp, Crossing:`Sliding Tackle`, -`Name Pos`) %>% 
    ggplot(aes(Skill, Exp))+
    geom_col(fill = fill_variable)+
    coord_flip()+
    theme_minimal()+
    facet_wrap(~(df %>% pull(`Name.Pos`)))+
    labs(x = NULL, y = "Ability")+
    theme(strip.background =element_rect(fill=fill_strip,color = "black"),
          strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic"))
  
  return(res)
  
}

facetReactiveLine <- function(df, player, color_variable, fill_strip){
  
  res <- NULL
  
  if(missing("df") | missing("player") | missing("color_variable") | missing("fill_strip")) return(res)
  if(is.null(df) | is.null("player") | is.null(color_variable) | is.null("fill_strip")) return(res)
  
  p1 <- df %>% filter(Name.Pos == player$Name.Pos, fifa == "FIFA 19") %>% pull(sofifa_id)
  p1 <- df %>% filter(sofifa_id == p1)%>% rename("Year" = fifa, "Value" = value_eur)
  
  res <- ggplot(p1)+
    geom_line(aes(x = Year, y = Value, group = 1), color = color_variable)+
    facet_wrap(~p1$Name.Pos[length(p1$Name.Pos)])+
    theme_minimal()+
    theme(legend.position='none',
          strip.background =element_rect(fill=fill_strip),
          strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic"))+
    labs(x = NULL, y = "Value, €")+
    scale_y_continuous(labels = comma)
  
  return(res)
  
}
  


