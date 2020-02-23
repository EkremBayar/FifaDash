# Description -------------------------------------------------------------

# This R file contains helper functions for inputs.


# Tooltips for Observe 


# 1. Tooltip Get Leagues --------------------------------------------------


get_league <- function(df, x = "League"){
  
  res <- NULL
  
  if(is.null(x)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df[,x] %>% unique() %>% sort()
  
  return(res)
}

get_league_sample <- function(df, x = "League"){
  
  res <- NULL
  
  if(is.null(x)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df[,x] %>% unique() %>% sort()
  res <- sample(res,1)
  
  return(res)
}


# 2. Tooltip Get Teams ----------------------------------------------------



get_team <- function(df, x){
  
  res <- NULL
  
  if(missing("x") ) return(res)
  if(is.null(x)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df %>% filter(League %in% x) %>% mutate(Club = as.character(Club))
  res <- res[,"Club"] %>% unique() %>% sort()
  return(res)
}

get_team_sample <- function(df, x){
  
  res <- NULL
  
  if(missing("x") ) return(res)
  if(is.null(x)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df %>% filter(League %in% x) %>% mutate(Club = as.character(Club))
  res <- res[,"Club"] %>% unique() %>% sort()
  res <- sample(res,1)
  return(res)
}

# 3. Tooltp Get Players ---------------------------------------------------
get_player <- function(df, x, y){

  res <- NULL
  
  if(missing("x") ) return(res)
  if(is.null(x)) return(res)
  if(missing("y") ) return(res)
  if(is.null(y)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df %>% filter(League %in% x, Club %in% y) %>% pull(Name.Pos)
  return(res)
}

get_player_sample <- function(df, x, y){
  
  res <- NULL
  
  if(missing("x") ) return(res)
  if(is.null(x)) return(res)
  if(missing("y") ) return(res)
  if(is.null(y)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  
  res <- df %>% filter(League %in% x, Club %in% y) %>% pull(Name.Pos)
  res <- sample(res,1)
  return(res)
}


# 4. Tooltip Transfer Page ------------------------------------------------



get_transfer <- function(df, funs, var){
  
  res <- NULL
  
  if(missing("funs") ) return(res)
  if(is.null(funs)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)
  if(missing("var") ) return(res)
  if(is.null(var)) return(res)
  
  res <- funs(df[,var], na.rm = TRUE)
  
  return(res)
}


# 5. Tooltip for X & Y Variables ------------------------------------------

get_variables <- function(df, league, class){
  
  res <- NULL
  
  if(missing("league") ) return(res)
  if(is.null(league)) return(res)
  if(missing("class") ) return(res)
  if(is.null(class)) return(res)
  if(missing("df") ) return(res)
  if(is.null(df)) return(res)

  corp <- df %>% filter(League == league, Class == class)
  
  if(class == "Goal Keeper"){
    
    corp <- corp %>% select(Agility, Reactions, Jumping, Strength, Vision, Composure, GK.Diving:GK.Reflexes)
    
  }else if(class == "Defender"){
    
    corp <- corp %>% select(Crossing, Heading.Accuracy, Agility, Reactions, Balance, Jumping, Stamina,
                            Strength, Aggression, Interceptions, Positioning, Vision,Composure, 
                            Marking, Standing.Tackle, Sliding.Tackle)
    
  }else if(class == "Midfielder"){
    
    corp <- corp %>% select(Crossing:Sliding.Tackle)
    
  }else{
    
    corp <- corp %>% select(Crossing:Aggression, Positioning:Composure)
    
  }
  res <- names(corp) %>% sort()
  
  return(res)
}


# 6. Get Picker List ------------------------------------------------------

get_picker_list <- function(x){
  
  res <- NULL
  
  if(missing("x") ) return(res)
  if(is.null(x)) return(res)
  
  res <-  as.list(setNames(x, x %>% gsub("[[:punct:]]", " ", .) ))
  
  return(res)
}


