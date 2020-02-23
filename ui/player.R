tabItem(
  tabName = "tab_players",
  
  column(
    width = 12,
    
    box(
      title = tagList(icon("bullseye"), "Player Stats"), status = "success", width=NULL, solidHeader = TRUE, collapsible = FALSE,
      
      tags$hr(),
      
      fluidRow(
        
        column(
          width = 2,
          boxPad(
            color = "red",
            h6("Player 1")),
          pickerInput("tp_league", "League:", choices = ""),
          pickerInput("tp_team", "Team:", choices =""),
          pickerInput("tp_player", "Player:", choices ="")
          
        ),
        
        column(
          width = 8,

          fluidRow(
            column(width = 4),
            column(width = 2, uiOutput("PlayerImg")), 
            column(width = 2,offset = 1, uiOutput("PlayerImg2")),
            column(width = 4)
            ),
          
          br(),
          conditionalPanel(condition = "output.PlayerImg",hr()),
          
          column(offset = 1, width = 12,
                 fluidRow(
                   valueBoxOutput("tp_age",width = 3),
                   valueBoxOutput("tp_overall",width = 2),
                   valueBoxOutput("tp_value",width = 2),
                   valueBoxOutput("tp_contract",width = 3)
                   ),

                 fluidRow(
                   valueBoxOutput("tp_age2",width = 3),
                   valueBoxOutput("tp_overall2",width = 2),
                   valueBoxOutput("tp_value2",width = 2),
                   valueBoxOutput("tp_contract2",width = 3)
                 )
          )
          
          ),
        
        column(
          width = 2,
          boxPad(
            h6("Player 2"),
            color = "blue"),
          pickerInput("tp_league2", "League:", choices = ""),
          pickerInput("tp_team2", "Team:", choices =""),
          pickerInput("tp_player2", "Player:", choices ="")
          )
        
      ),
      
      tags$hr(),
      tags$br(),
      
      conditionalPanel(
        condition = "input.start",
        
        column(
          width = 12,
          
          tabsetPanel(type = "pills",
            
            tabPanel("Radar",
                     br(), withSpinner(plotlyOutput("tp_radar"))),
                   
            tabPanel("Bar",
                     br(), withSpinner(plotOutput("tp_bar")), plotOutput("tp_bar2")),
                   
            tabPanel("Line",
                     br(), withSpinner(plotOutput("tp_line"))),
                   
            tabPanel("Similarity",
                     br(), 
                     
                     fluidRow(
                       column(width = 12,
                              prettyRadioButtons(inputId = "distance",inline = TRUE,
                                                 shape = "curve",status = "success", 
                                                 label = "Select Distance Measure:",
                                                 choices = c("Euclidean", "Maximum",
                                                             "Manhattan", "Canberra",
                                                             "Minkowski", "Pearson",
                                                             "Spearman", "Kendall")))
                     ),
                     
                     fluidRow(
                       column(width = 3,
                              pickerInput(inputId = "sm_league", label = "Select League:",
                                          choices =  sort(c("Bundesliga", "La Liga", "Premier League", 
                                                            "Serie A", "Süper Lig", "Ligue 1", 
                                                            "Liga Nos", "Eredivisie")),
                                          multiple = T,
                                          selected = c("Bundesliga", "La Liga", "Premier League"),
                                          options =  list(
                                            "max-options" = 3,
                                            "max-options-text" = "Max 3 League!",
                                            "none-selected-text" = "Please make a selection!"
                                          )
                              )
                       )
                     ),
                     br(), 
                     fluidRow(
                       withSpinner(plotOutput("similarityplot"))
                     ))
          )
                   
        )
      )
      
      
      )))
  
  
  

           