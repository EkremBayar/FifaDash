# 1. PACKAGES -------------------------------------------------------------
source("library/library.R")

# 2. CONFIGURATION --------------------------------------------------------

options(scipen = 999)

# 3. FUNCTIONS ------------------------------------------------------------

source("server/2_functions.R") 

# 4. HEADER ---------------------------------------------------------------

header <- dashboardHeaderPlus(title = tags$img(src='fifalogo.png', height = 28), 
                              enable_rightsidebar = FALSE, titleWidth = 250)


# 5. SIDEBAR --------------------------------------------------------------

sidebar <- tagList(
    sidebarMenu(id="tabs",
                menuItem("Home", tabName = "tab_home", icon = icon("home")), 
                menuItem("Leagues", tabName = "tab_leagues", icon = icon("futbol")),
                menuItem("Teams", tabName = "tab_teams", icon = icon("users")),
                menuItem("Players", tabName = "tab_players", icon = icon("walking")),
                menuItem("Scout", tabName = "tab_scout", icon = icon("address-card")), # crosshairs
                menuItem("Developer", tabName = "tab_dev", icon = icon("child")),
                menuItem("Videos", tabName = "tab_video", icon = icon("youtube"))
    )
)

# 6. BODY -----------------------------------------------------------------

body <- tagList(br(), useShinyjs(),useShinyalert(),
                # Slider Color
                chooseSliderSkin(skin="Modern", color = "rosybrown"),
                

                tabItems(

                  # Home
                  source(file.path("ui", "home.R"),  local = TRUE, encoding = "UTF-8" )$value,

                  # League
                  source(file.path("ui", "league.R"),  local = TRUE, encoding = "UTF-8" )$value,
                  
                  # Team
                  source(file.path("ui", "team.R"),  local = TRUE, encoding = "UTF-8" )$value,
                   
                  # Player
                  source(file.path("ui", "player.R"),  local = TRUE, encoding = "UTF-8" )$value,
                  
                  # Scout
                  source(file.path("ui", "scout.R"),  local = TRUE, encoding = "UTF-8" )$value,
                   
                  # Developer
                  source(file.path("ui", "developer.R"),  local = TRUE, encoding = "UTF-8" )$value,
                  
                  # Video
                  source(file.path("ui", "video.R"),  local = TRUE, encoding = "UTF-8" )$value

                )
)


# 7. UI -------------------------------------------------------------------

ui <-  dashboardPagePlus(
  
  title="FIFA 19 DS & ML Applications", skin = "green",
    
  # Header
  header,
    
  # Sidebar
  dashboardSidebar(
    width = 220,br(), 
    #tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")), # Header Toggle
    uiOutput("mySidebarUI")
    ),
  
  # Body
  dashboardBody(
    uiOutput("myBodyUI"),
    tags$head(
      
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), # CSS
      tags$style(HTML("input[type='search']:disabled {visibility:hidden}"),
                 HTML(".shiny-output-error { visibility: hidden; }"), # Hiding Errors & Warnings
                 HTML(".shiny-output-error:before { visibility: hidden; }"),
                 HTML('.small-box .icon-large {font-size: 500%;}'), # Alperene Sor?
                 HTML(".navbar-default {background-color: darkslategrey; border-color: white;}"),
                 HTML('.navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover {color: seagreen;background-color: white;}'),
                 HTML(".navbar-default .navbar-nav>li>a:focus, .navbar-default .navbar-nav>li>a:hover {color: white;background-color: seagreen;}"),
                 HTML(".navbar-default .navbar-nav>li>a {color: white;}"),
                 HTML(".skin-green .sidebar-menu>li.active>a {border-left-color: goldenrod;}"),
                 HTML(".skin-green .sidebar-menu>li.active>a, .skin-green .sidebar-menu>li.menu-open>a, .skin-green .sidebar-menu>li:hover>a {color: #fff;background: seagreen;}"),
                 HTML(".pl1, .alert-success, .bg-green, .callout.callout-success, .label-success, .modal-success .modal-body {background-color: crimson;}"),
                 HTML(".bg-gray {color: white;background-color: mediumseagreen;}"),
                 #HTML(".nav-pills>li.active>a, .nav-pills>li.active>a:focus, .nav-pills>li.active>a:hover {color: #fff;background-color: seagreen;}"),
                 HTML(".nav-pills>li.active>a, .nav-pills>li.active>a:focus, .nav-pills>li.active>a:hover {border-top-color: darkslategrey;}"),
                 HTML(".alert-success, .bg-green, .callout.callout-success, .label-success, .modal-success .modal-body {background-color: darkseagreen!important;}")
                 ),
      tags$link(rel = "shortcut icon", href = "fifaicon.ico") # Favicon
      )
    ),
  
  # Footer
  tags$footer(class="main-footer",
              tagList(
                span(style="", "Developed by",tags$strong("Ekrem BAYAR")," 2019, ",tags$strong("version"), "8.0.0"),
                span(style="float:right;",tags$img(src="fifa-19-logo.png", height=27))
                )
              )
  )



# 8. SERVER ---------------------------------------------------------------

server <- function(input, output, session) {
  
  # Session
  session$onSessionEnded(stopApp)
  
  # Render UI
  observe({
    output$mySidebarUI <- renderUI({ sidebar })
    output$myBodyUI <- renderUI({  body })
    
    isolate({updateTabItems(session, "tabs", "tab_home")})
    
  })
  
  # 7.2. Server Operations --------------------------------------------------
  
  
  # 1. Data & Manipulation
  source(file.path("server", "1_tidy_data.R"),  local = TRUE, encoding = "UTF-8")$value
  
  # 2. Tooltips
  source("server/3_tooltip.R") # Tooltips for Observe
  
  # 3. Observe
  source(file.path("server", "4_observe.R"),  local = TRUE, encoding = "UTF-8")$value
  
  # 4. League
  source(file.path("server", "5_league.R"),  local = TRUE, encoding = "UTF-8")$value
  
  # # 5. Team
  source(file.path("server", "6_team.R"),  local = TRUE, encoding = "UTF-8")$value
  
  # 6. Player
  source(file.path("server", "7_player.R"),  local = TRUE, encoding = "UTF-8")$value
   
  # 7. Scout
  source(file.path("server", "8_scout.R"),  local = TRUE, encoding = "UTF-8")$value
  
}


# 9. SHINY APP ------------------------------------------------------------

shinyApp(ui, server)   