tabItem(
  tabName = "tab_teams",
  
  column(
    width = 12,
    
    box(
      title = tagList(icon("bullseye"), "Team Stats"), status = "success", width=NULL, solidHeader = TRUE, collapsible = FALSE,
      
      tags$hr(),
      
      fluidRow(
        
        column(
          width = 2,
          pickerInput("tt_league", "League:", choices = ""),
          pickerInput("tt_team", "Team:", choices =""),
          disabled(actionBttn(inputId = "tt_select", tagList(icon("location-arrow"),"Select"), 
                              size = "sm", color = "success", style = "simple", block = T))
        ),
        
        column(width = 10,
               tags$img(src="pl.png", style = "height: 70%; width: 10%;margin-left: 15%;"),
               tags$img(src="laliga.png", style = "height: 80%; width: 12%; margin-left: 10px;"),
               tags$img(src="bundesliga.png", style = "height: 10%; width: 6%; margin-left: 10px;"),
               tags$img(src = "seriea.png", style = "height: 10%; width: 4.8%; margin-left: 10px;"),
               tags$img(src = "ligue1.png", style = "height: 20%; width: 4.5%; margin-left: 10px;"),
               tags$img(src = "superlig.png", style = "height: 20%; width: 5%; margin-left: 10px;"),
               tags$img(src = "liganos.png", style = "height: 20%; width: 4.5%; margin-left: 10px;"),
               tags$img(src = "eredivisie.png", style = "height: 20%; width: 8%; margin-left: 10px;")
        )
        
      ),
      
      tags$br(),
      tags$hr(),
      tags$br(),
      
      column(
        width = 12,
        
        conditionalPanel(condition = "input.tt_select",
                         
                         tabsetPanel(type = "pills",
                                    
                                    tabPanel("Summary",
                                             br(),
                                             withSpinner(plotOutput("tt_summary",height = "700px"))),
                                    
                                    tabPanel("Value", 
                                             br(),
                                             withSpinner(plotOutput("tt_value")),
                                             br(),
                                             withSpinner(plotOutput("tt_value2")),
                                             br(),
                                             withSpinner(plotlyOutput("tt_value3")),
                                             br(), 
                                             withSpinner(plotlyOutput("tt_3d",height = "700px"))),
                                    
                                    tabPanel("Best Players", 
                                             br(),
                                             prettyRadioButtons(inputId = "tt_tactic", 
                                                                label = "Best Players in Team in terms of Tactics:",
                                                                choices = c("4-4-2", "3-5-2", "4-3-3"), 
                                                                shape = "curve",
                                                                status = "success", 
                                                                selected = "4-3-3",
                                                                inline = TRUE),
                                             column(width = 6, withSpinner(tableOutput("tt_best_team"))),
                                             
                                             column(width = 6, plotOutput("tt_best_team_plot"))
                                             ),
                                    
                                    tabPanel("Stats", 
                                             br(),
                                             column(width = 5,withSpinner(tableOutput("tt_stats"))),
                                             column(width = 7, withSpinner(d3heatmapOutput("tt_heatmap"))),
                                             br(), br()),
                                    
                                    tabPanel("Set Piece Goal", 
                                             br(),
                                             column(width = 5, offset = 1, h4("Free Kick Takers"), withSpinner(tableOutput("tt_spgoal1"))),
                                             column(width = 5, h4("Penalty Takers"), withSpinner(tableOutput("tt_spgoal2")))
                                    ),
                                    
                                    tabPanel("BMI", 
                                             br(),
                                             column(width = 6, withSpinner(tableOutput("tt_bmi"))),
                                             column(width = 6, withSpinner(plotOutput("tt_bmi2"))))
                                    )
                         )
        )
      
      )
    
    
  
  
  )
  
  
  
  )