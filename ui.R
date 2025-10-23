# ui.R
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)

ui <- dashboardPage(
  dashboardHeader(
    title = "🎲 Unfair Flips",
    tags$li(class = "dropdown", style = "padding:10px;", uiOutput("currentStatus"))
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Play", tabName = "play", icon = icon("gamepad"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    useSweetAlert(),  # <-- needed for sendSweetAlert()
    
    # Google Fonts + custom CSS + keyboard shortcuts
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Roboto&family=Bangers&display=swap"),
      tags$style(HTML("
        body { font-family: 'Roboto', sans-serif; }
        h2,h3,h4,h5,h6 { font-family: 'Bangers', cursive; letter-spacing: .5px; }
        .progress { height: 30px; }
        .upgrade-btn { width: 100%; margin-bottom: 6px; }
        .box { box-shadow: 0 1px 4px rgba(0,0,0,.08); }
        /* Coin animation */
        .coin {
          width: 80px; height: 80px; border-radius: 50%;
          background: radial-gradient(circle at 30% 30%, #ffd54f, #f9a825);
          box-shadow: inset 0 1px 3px rgba(0,0,0,.25), 0 4px 10px rgba(0,0,0,.1);
          display: inline-block; margin-left: 12px; vertical-align: middle;
        }
        .spinning { animation: flip 0.8s linear infinite; }
        @keyframes flip { from {transform: rotateY(0deg);} to {transform: rotateY(360deg);} }
        .kpi-row .small-box { margin-bottom: 10px; }
        .cost-badge { font-weight: 600; }
      ")),
      # Keyboard shortcuts: F = flip, R = reset
      tags$script(HTML("
        document.addEventListener('keydown', function(e) {
          if (['INPUT','TEXTAREA','SELECT'].includes((document.activeElement||{}).tagName)) return;
          if (e.key === 'f' || e.key === 'F') { Shiny.setInputValue('flip', Math.random(), {priority: 'event'}); }
          if (e.key === 'r' || e.key === 'R') { Shiny.setInputValue('reset', Math.random(), {priority: 'event'}); }
        });
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
          # KPI row
          class = "kpi-row",
          valueBoxOutput("vbPoints", width = 2),
          valueBoxOutput("vbStreak", width = 2),
          valueBoxOutput("vbBestStreak", width = 2),
          valueBoxOutput("vbHeadChance", width = 3),
          valueBoxOutput("vbFlipDelay", width = 3)
        ),
        
        fluidRow(
          # Left column
          column(
            width = 6,
            box(width = NULL, title = "Game", status = "primary", solidHeader = TRUE,
                div(
                  style = "display:flex; align-items:center; gap:12px;",
                  actionButton("flip", "Flip Coin", class = "btn-primary btn-lg"),
                  actionButton("reset", "Reset Game", class = "btn-danger btn-sm")
                ),
                br(),
                h4("Result"),
                textOutput("result"),
                div(id = "coin", class = "coin", title = "Coin"),
                br(), br(),
                h4("Streak Progress"),
                progressBar(
                  id = "streakPB", value = 0, total = 10,
                  display_pct = TRUE, striped = TRUE, status = "danger"  # removed animated
                )
            ),
            box(width = NULL, title = "Stats", status = "info", solidHeader = TRUE,
                tableOutput("statsTable")
            )
          ),
          
          # Right column
          column(
            width = 6,
            box(width = NULL, title = "Upgrades", status = "success", solidHeader = TRUE,
                uiOutput("upgradeUI")
            ),
            box(width = NULL, title = "Flip Log", status = "warning", solidHeader = TRUE,
                DTOutput("logDT")
            )
          )
        )
      )
    )
  )
)
