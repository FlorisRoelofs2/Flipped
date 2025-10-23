ui <- dashboardPage(
  dashboardHeader(
    title = "🎲 Unfair Flips",
    tags$li(class = "dropdown",
            style = "padding:10px;",
            uiOutput("currentStatus"))
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Play", tabName = "play", icon = icon("gamepad"))
    )
  ),
  
  dashboardBody(
    # Google Fonts
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto&family=Bangers&display=swap"),
      tags$style(HTML("
        body { font-family: 'Roboto', sans-serif; }
        h2,h3,h4,h5,h6 { font-family: 'Bangers', cursive; }
        .progress { height: 30px; }
        .upgrade-btn { width: 100%; margin-bottom: 5px; }
      "))
    ),
    
    tabItems(
      # ---- Introduction ----
      tabItem(
        tabName = "intro",
        fluidRow(
          box(width = 12, title = "Welcome to Unfair Flips!", status = "primary", solidHeader = TRUE,
              h4("How it Works:"),
              tags$ul(
                tags$li("Click 'Flip Coin' to flip the coin."),
                tags$li("Each heads adds points based on your current reward and streak multiplier."),
                tags$li("Tails resets your current streak back to zero."),
                tags$li("Try to get 10 heads in a row to win!")
              ),
              h4("Upgrades and Strategy:"),
              tags$ul(
                tags$li("⭐ Increase Head Chance (+5%)"),
                tags$li("⚡ Decrease Flip Time (-0.2s)"),
                tags$li("💰 Increase Reward per Head (+1 point)"),
                tags$li("📈 Streak Multiplier (+0.2×)"),
                tags$li("⚠️ Buying an upgrade resets your current streak")
              ),
              h4("Challenge Yourself:"),
              tags$ul(
                tags$li("Try to beat the game in fewer than 300 flips or under 500 seconds."),
                tags$li("Keep an eye on your streak progress at the top.")
              )
          )
        )
      ),
      
      # ---- Play ----
      tabItem(
        tabName = "play",
        fluidRow(
          # Left column: game controls & stats
          column(width = 6,
                 box(width = NULL, title = "Game Controls", status = "primary", solidHeader = TRUE,
                     actionButton("flip", "Flip Coin", class = "btn-primary btn-lg"),
                     actionButton("reset", "Reset Game", class = "btn-danger btn-sm", style = "margin-left:10px;"),
                     br(), br(),
                     h4("Result"),
                     textOutput("result"),
                     br(),
                     h4("Streak Progress"),
                     uiOutput("streakBar")
                 ),
                 box(width = NULL, title = "Stats", status = "info", solidHeader = TRUE,
                     tableOutput("statsTable")
                 )
          ),
          
          # Right column: Upgrades & log
          column(width = 6,
                 box(width = NULL, title = "Upgrades", status = "success", solidHeader = TRUE,
                     uiOutput("upgradeUI")
                 ),
                 box(width = NULL, title = "Flip Log", status = "warning", solidHeader = TRUE,
                     tags$div(style = "height:200px; overflow-y:scroll; background:#f8f9fa; padding:10px; border:1px solid #ddd;",
                              verbatimTextOutput("log"))
                 )
          )
        )
      )
    )
  )
)
