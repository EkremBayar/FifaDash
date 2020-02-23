tabItem(
  tabName = "tab_scout",
  
  column(
    width = 12,
    
    box(
      title = tagList(icon("bullseye"), "Scout"), status = "success", width=NULL, solidHeader = TRUE, collapsible = FALSE,
      
      tags$hr(),
      
      navbarPage(
        title = NULL,collapsible = TRUE, theme = "green", 
        
        tabPanel("Player Database", icon = icon("database"),
                 conditionalPanel(condition = "input.start",
                                  withSpinner(dataTableOutput("scout_dt", width = "100%")))),
        
        tabPanel("Best Players", icon = icon("street-view"),
                 conditionalPanel(condition = "input.start",
                                  
                                  fluidRow(
                                    uiOutput("GoalKeeper"),
                                    uiOutput("Defender"),
                                    uiOutput("Midfielder"),
                                    uiOutput("Forward")
                                  )
                                  )
                 ),
        
        tabPanel("Player Stats", icon = icon("dumbbell"),
                 conditionalPanel(condition = "input.start",
                                  
                                  fluidRow(
                                    column(width = 6,
                                           fluidRow(
                                             column(width = 4, pickerInput("ts_league",label = "League:", choices = "")),
                                             column(width = 4, pickerInput("ts_team",label = "Team:", choices = "")),
                                             column(width = 4, pickerInput("ts_player",label = "Player:", choices = ""))
                                           ),
                                           fluidRow(
                                             column(width = 12,
                                                    actionBttn(inputId = "ts_select", tagList(icon("location-arrow"),"Select"), size = "sm", color = "success", 
                                                               style = "simple", block = T))
                                           )
                                    ),
                                    
                                    column(width = 6,
                                           fluidRow(
                                             conditionalPanel(condition = "input.ts_select",
                                                              h3("SWOT Analysis")
                                                              )
                                             ),
                                           fluidRow(
                                             conditionalPanel(condition = "input.ts_select",
                                                              column(width = 6, h5("Strengths")),
                                                              column(width = 6, h5("Weaknesses"))
                                                              )
                                             )
                                           )
                                  ),
                                  
                                  br(), 
                                  
                                  fluidRow(
                                    uiOutput("ts_player_photo"),
                                    column(width = 3,uiOutput("ts_slist")),
                                    column(width = 3,uiOutput("ts_slist2"))
                                    ),
                                  
                                  hr(),br(),
                                  
                                  conditionalPanel(condition = "input.ts_select",
                                                   
                                                   tabsetPanel(
                                                     tabPanel("Player Data",
                                                              br(),
                                                              withSpinner(dataTableOutput("ts_player_stats"))),
                                                     
                                                     tabPanel("Player Visualization",
                                                              column(width = 4,
                                                                     withSpinner(plotOutput("ts_player_viz"))),
                                                              column(width = 4,
                                                                     withSpinner(plotOutput("ts_playerPosPower"))),
                                                              column(width = 4,
                                                                     withSpinner(plotOutput("ts_dend")))
                                                              ),
                                                     
                                                     tabPanel("Linear Discriminant Analysis",
                                                              withSpinner(plotOutput("ldaplot"))
                                                     )
                                                   )
                                  )
                 )
                 ),


        tabPanel("Player Clustering", icon = icon("cubes"),
                 
                 conditionalPanel(condition = "input.start",
                                  
                                  wellPanel(
                                    fluidRow(
                                      column(width = 2, 
                                             pickerInput("cl_cluster", label = "Select Position:", choices = "")
                                      ),
                                      column(width = 10, 
                                             h3("Clustering by Class")
                                      )
                                    )
                                  ),
                                  
                                  hr(),
                                  
                                  fluidRow(
                                    column(width = 6, withSpinner(plotOutput("cluster"))),
                                    column(width = 6, withSpinner(plotOutput("opplot")))
                                  ),
                                  
                                  br(),
                                  
                                  fluidRow(
                                    tabsetPanel(
                                      tabPanel("Cluster Summary",
                                               dataTableOutput("clusterMean")),
                                      tabPanel("Player Cluster",
                                               dataTableOutput("clusterdt"))
                                    )
                                  )
                                  
                                  )
                 
                 ),
        
        tabPanel("PCA", icon = icon("poll"),
                 
                 conditionalPanel(condition = "input.start",
                                  
                                  wellPanel(
                                    fluidRow(
                                      column(width = 2, 
                                             pickerInput("pca_class", label = "Select Position:", choices = "")
                                      ),
                                      column(width = 10, 
                                             h3("Principle Component Analysis")
                                      )
                                    )
                                  ),
                                  
                                  hr(),
                                  
                                  fluidRow(
                                    column(width = 7, dataTableOutput("pca_players")),
                                    column(width = 5, dataTableOutput("pca_clubs"))
                                  ),
                                  
                                  fluidRow(
                                    plotOutput("pca_players_plot")
                                  ),
                                  
                                  fluidRow(
                                    column(width = 6, plotOutput("pca_players_barplot")),
                                    column(width = 6, plotOutput("pca_players_barplot2"))
                                  )
                                  
                                  )
                 ),
        
        tabPanel("Correlation", icon = icon("braille"),
                 
                 conditionalPanel(condition = "input.start",
                                  
                                  fluidRow(
                                    column(width = 8,
                                           fluidRow(
                                             column(width = 3, pickerInput("cr_league",label = "League:", choices = "")),
                                             column(width = 3, pickerInput("cr_class",label = "Position:", choices = "")),
                                             column(width = 3, pickerInput("cr_x",label = "X Variable:", choices = "")),
                                             column(width = 3, pickerInput("cr_y",label = "Y Variable:", choices = ""))
                                             )
                                           ),
                                    column(width = 4,
                                           fluidRow(
                                             column(width = 4,
                                                    div(style = "margin-top:25px;",
                                                        actionBttn(inputId = "cr_select", tagList(icon("location-arrow"),"Select"), size = "sm", 
                                                                   color = "success", style = "simple", block = T))))
                                           )
                                    ),
                                  
                                  hr(),
                                  
                                  conditionalPanel(condition = "input.cr_select",
                                                   
                                                   fluidRow(plotOutput("cr_corp")),
                                                   hr(),
                                                   fluidRow(h3("Unpaired Two-Samples Wilcoxon Test"),
                                                            p("Is there any significant difference between left foot and right foot abilities?"),
                                                            plotOutput("cr_htest"))
                                                   
                                                   )
                                  
                                  )
                 ),
        
        tabPanel("Player Searching", icon = icon("binoculars"),
                 
                 conditionalPanel(condition = "input.start", 
                                  
                                  wellPanel(
                                    
                                    fluidRow(
                                      
                                      column(width = 3, 
                                             sliderInput("s_value", label = "Value Range:",
                                                         min = 0, max = 120000000, value = c(20000000,60000000)),
                                             sliderInput("s_age", label = "Age Range:",
                                                         min = 15, max = 45, value = c(26,30)),
                                             sliderInput("s_overall", label = "Overall Range:",
                                                         min = 0, max = 100, value = c(75,94))
                                      ),
                                      
                                      column(width = 1),
                                      
                                      column(width = 2,
                                             
                                             pickerInput(inputId = "s_class", label = "Class:", 
                                                         choices = c("Defender", "Forward", "Goal Keeper", "Midfielder"),multiple = T),
                                             pickerInput(inputId = "s_league", label = "League:", 
                                                         choices = c("Bundesliga", "Eredivisie","La Liga","Liga Nos", "Ligue 1", "Premier League", "Serie A", "Süper Lig"),multiple = T),
                                             pickerInput(inputId = "s_cluster", label = "Cluster:", 
                                                         choices =  c("Cluster 1" = 1, "Cluster 2" = 2, "Cluster 3" = 3, "Cluster 4" = 4), 
                                                         selected = 1:4,multiple = T),
                                             pickerInput(inputId = "s_prfoot", label = "Preferred Foot:", 
                                                         choices =  c("Left", "Right"),
                                                         selected = c("Left", "Right"),multiple = T),
                                             pickerInput(inputId = "s_inrep", label = "International Reputation:", 
                                                         choices =  1:5, selected = 1:5,multiple = T)
                                             
                                             ),
                                      
                                      column(width = 1),
                                      
                                      column(width = 5,
                                             
                                             fluidRow(
                                               column(width = 6,
                                                      
                                                      textInput("text", label = "Transfer Player"),
                                                      actionBttn(inputId = "trans", "Transfer", size = "sm", color = "success", 
                                                                 style = "simple", block = T),
                                                      br(),
                                                      disabled(actionBttn(inputId = "sell", "Sell", size = "sm", color = "success", 
                                                                          style = "simple", block = T))
                                                      
                                               )
                                             ),
                                             
                                             br(),
                                             
                                             fluidRow(
                                               column(width = 6,
                                                      
                                                      infoBox("Budget", "€200.000.000", icon = icon("donate"),
                                                              color = "purple",width = 13),
                                                      infoBoxOutput("Wasted",width = 13)
                                                      
                                               )
                                             ))
                                      
                                       )
                                    
                                  ),
                                  
                                  hr(),
                                  br(), 
                                  
                                  tabsetPanel(
                                    tabPanel("Searched Players", br(), withSpinner(dataTableOutput("transfer"))),
                                    tabPanel("Your Squad", br(), dataTableOutput("transferPlayer"))
                                  )
                                  
                                  
                                  )
                 
                 
                 )
        
      )
      
      
      
      
    )
  )
  
)

