tabItem(
  tabName = "tab_home", 
  class = "active", # class = "active" argümanı açılış sayfası eklediğimiz için ilk ekran boş gözüktüğünden eklendi
  
  column(
    width = 12,
    
    fluidRow(
      
      # Welcome Box
      box(title = "Welcome to The FIFA 19 Dashboard", status = "success", width = NULL, solidHeader = TRUE, collapsible = FALSE,
          
          # Ronaldo FIFA 19 Image
          column(width = 8, tags$img(src="fifa.jpg", style="width: 100%; height: 100%;")),
          
          # Description
          column(width = 4, 
                 tags$img(src="pl.png", style = "height: 70%; width: 35%"),
                 tags$img(src="laliga.png", style = "height: 80%; width: 40%"),
                 tags$img(src="bundesliga.png", style = "height: 10%; width: 20%"),
                 tags$br(), tags$br(),
                 tags$hr(),
                 tags$img(src = "seriea.png", style = "height: 10%; width: 14%; margin-left: 5px;"),
                 tags$img(src = "ligue1.png", style = "height: 20%; width: 14%; margin-left: 5px;"),
                 tags$img(src = "superlig.png", style = "height: 20%; width: 14%; margin-left: 5px"),
                 tags$img(src = "liganos.png", style = "height: 20%; width: 14%; margin-left: 5px;"),
                 tags$img(src = "eredivisie.png", style = "height: 20%; width: 23%; margin-left: 5px;"),
                 tags$br(), tags$br(),
                 tags$hr(),
                 fluidRow(column(width = 4), 
                          column(width = 4, tags$img(src="respectuefa.png", style = "height: 5%; width: 100%")),
                          column(width = 4)),
                 tags$br(),
                 tags$hr(),
                 #tags$p("This Shiny Dashboard designed for comparison of the teams and players stats of Bundesliga, La Liga and Premier League in FIFA 19."),
                 tags$p("The dashboard that I designed serves the purpose to enhance the data science experience in sports analytics with FIFA 19 dataset. After the updates, the dashboard now contains lots of analysis, EDAs  and visuals to compare leagues, teams and players. It also helps to discover new talents."),
                 
                 br(), 
                 fluidRow(
                   # Start Dashboard
                   tags$p("Please click to start!", style = "margin-left: 40%;"),
                   div(style = "margin-left: 33%;",actionBttn("start", label = "Become A Legend!", color = "success"))
                 )
          )
      )
      )
    )
  

  
  
         
         
             
             
  
)

