tabItem(
  tabName = "tab_video",
  
  
  box(title = tagList(icon("playstation"),"FIFA 19 Videos"), status = "success", width = NULL, solidHeader = TRUE, collapsible = FALSE,
      
      fluidRow(
        column(
          width = 6,
          HTML('<iframe src="https://www.youtube.com/embed/zX0AV6yxyrQ" width="100%" height="450" frameborder="0" allowfullscreen></iframe>')
        ),
        
        column(
          width = 6,
          HTML('<iframe src="https://www.youtube.com/embed/qTz8ZhNrEDA" width="100%" height="450" frameborder="0" allowfullscreen></iframe>')
        )
      ),
      
      br(), 
      
      fluidRow(
        column(
          width = 6,
          HTML('<iframe src="https://www.youtube.com/embed/OumZxTdMq_c" width="100%" height="450" frameborder="0" allowfullscreen=""></iframe>')
        ),
        column(
          width = 6,
          HTML('<iframe src="https://www.youtube.com/embed/9JBkGJ8P2m8?start=18" width="100%" height="450" frameborder="0" allowfullscreen=""></iframe>')
        )
      )
  )
  
  
  
)
