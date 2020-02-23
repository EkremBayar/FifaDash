tabItem(
  tabName = "tab_leagues",
  
  column(
    width = 12,
    
    box(
      title = tagList(icon("bullseye"), "League Stats"), status = "success", width=NULL, solidHeader = TRUE, collapsible = FALSE,
      
      tags$hr(),
      
      fluidRow(
        
        column(
          width = 2,
          pickerInput("tl_league", "League:", choices = c("")),
          disabled(actionBttn(inputId = "tl_select", tagList(icon("location-arrow"),"Select"), 
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
        
        conditionalPanel(condition = "input.tl_select",
                         
                         fluidRow(
                           valueBoxOutput("values"),
                           valueBoxOutput("numofplayers"),
                           valueBoxOutput("teams")
                           ),
                         
                         br(),
                         
                         fluidRow(
                           
                           conditionalPanel(
                             condition = "output.values",
                             
                             column(width = 6,
                                    div(style="clear:both;  margin-left: 20px;",
                                        prettyRadioButtons(inputId = "tl_tactic", 
                                                           label = "Best Players in The League in terms of Tactics:",
                                                           choices = c("4-4-2", "3-5-2", "4-3-3"), 
                                                           shape = "curve",
                                                           status = "success", 
                                                           inline = TRUE)
                                    ),
                                    withSpinner(tableOutput("best_team"))
                                    
                             ),
                             
                             column(width = 6,
                                    
                                    tabsetPanel(
                                      
                                      tabPanel("Nationality",
                                               br(),
                                               withSpinner(
                                                 plotlyOutput(outputId = "league_nat1"))
                                      ),
                                      
                                      tabPanel("Players", 
                                               br(),
                                               withSpinner(
                                                 plotOutput(outputId = "league_values", height = "100%")
                                                 )
                                      ),
                                      
                                      tabPanel("Comparison", 
                                               
                                               br(),
                                               
                                               fluidRow(
                                                 column(width = 6, pickerInput("comp_league", choices = c("League", "Team", "Position"))),
                                                 column(width = 6, pickerInput("comp_graph", choices = c("Bar", "Boxplot")))
                                               ),
                                               
                                               br(), withSpinner(plotOutput("league_comp1"))
                                      )
                                    )
                             )
                           )
                                            
                                            
                                            
                           )
                           
                           
                         
        )
        
      )
    )
    
  )
  
)

                           
                           
                           
                           
                           






