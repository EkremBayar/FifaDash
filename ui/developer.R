tabItem(tabName = "tab_dev",
        
        
        fluidRow(
          
          column(width = 6,
                 
                 widgetUserBox(
                   title = tags$em("Ekrem BAYAR"),
                   subtitle = tagList(
                     p(tags$strong("Developer")),
                     tags$strong("Data Scientist at Dogus Teknoloji")
                                      ),
                   type = 2,
                   width = 10,
                   src = "ekrem.png",
                   color = "green",
                   closable = FALSE,
                   footer_padding = FALSE,
                   collapsible = FALSE,
                   socialButton(
                     url = "https://www.linkedin.com/in/ekrem-bayar-3838aba9/",
                     type = "linkedin"),
                   socialButton(
                     url = "https://www.kaggle.com/ekrembayar",
                     type = "kaggle"
                     ),
                   socialButton(
                     url = "https://www.youtube.com/channel/UC6sRvi8MsLRpeYY35EuDNuw/videos",
                     type = "youtube")
                   )
                 
                 ),
          
          column(width = 6,
                 
                 widgetUserBox(
                   title = tags$em("Alperen BALIK"),
                   subtitle = tagList(
                     p(tags$strong("Contributor")),
                     tags$strong("Data Scientist")
                     ),
                   type = 2,
                   width = 10,
                   src = "alperen.jpeg",
                   color = "green",
                   closable = FALSE,
                   footer_padding = FALSE,
                   collapsible = FALSE,
                   socialButton(
                     url = "https://www.linkedin.com/in/alperen-bal%C4%B1k-057097142/",
                     type = "linkedin")
                 )
                 
          )
          
          
          )
        )


