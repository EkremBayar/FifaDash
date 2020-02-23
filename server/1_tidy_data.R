# Description -------------------------------------------------------------

# This R file is for the Home page in the dashboard. 
# Dashboard works when clicking "Become A Legend" button.
# FIFA data comes from the tidydata file in the FifaDash file.



# 1. Reactive Values ------------------------------------------------------


rv <- reactiveValues(df = NULL, # Data Frame
                     fifa_year = NULL, fifa_team_year = NULL # Other FIFA Games
                     )


# 2. ObserveEvent ---------------------------------------------------------


observeEvent(input$start,{
  
# 3. Pop-up EA Intro ------------------------------------------------------

    
  sendSweetAlert(
    session = session, type = NULL, closeOnClickOutside = FALSE, btn_labels = "Skip",
    tags$video(
      src = "ea_sports.mp4", width = "440px", height = "350px", type = "video/mp4", controls = NULL,autoplay = TRUE, allowfullscreen= FALSE
      ), 
    title = NULL
    )
  
# 4. Read Data ------------------------------------------------------------

  
  # Read FIFA 19 #
  temp <- read.csv("database/fifa19_data.csv", encoding = "UTF-8",stringsAsFactors = FALSE)[-1]
  rv$df <- temp
  
  # Read FIFA Series for Players (2016, 2017, 2018, 2019, 2020)
  f <- read.csv("database/fifa_series_players.csv", encoding = "UTF-8")[-1]
  rv$fifa_year <- f
  
  # Read FIFA Series for Teams (2016, 2017, 2018, 2019, 2020)
  f_t <- read.csv("database/fifa_series_teams.csv", encoding = "UTF-8")[-1]
  rv$fifa_team_year <- f_t

 
# 5. Transfer -------------------------------------------------------------
  
  write.csv(rv$df[0,], "database/transfer.csv", fileEncoding = "UTF-8")
  

# 6. Enable Select Buttons ------------------------------------------------

  
  enable("tl_select")
  enable("tt_select")
  enable("tp_select")
  
})

