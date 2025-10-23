# app.R
# Run with: shiny::runApp()

library(shiny)
library(shinydashboard)
library(DT)
library(later)

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
    # Google Fonts + custom CSS + keyboard shortcuts
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Roboto&family=Bangers&display=swap"
      ),
      tags$style(HTML("
        body { font-family: 'Roboto', sans-serif; }
        h2,h3,h4,h5,h6 { font-family: 'Bangers', cursive; letter-spacing: .5px; }
        .progress { height: 30px; }
        .box { box-shadow: 0 1px 4px rgba(0,0,0,.08); }
        .kpi-row .small-box { margin-bottom: 10px; }
        .cost { color: #666; font-weight: 600; }
        .btn-upg { width: 100%; margin-bottom: 6px; }
        /* Coin (no JS dependencies) */
        .coin {
          width: 80px; height: 80px; border-radius: 50%;
          background: radial-gradient(circle at 30% 30%, #ffd54f, #f9a825);
          box-shadow: inset 0 1px 3px rgba(0,0,0,.25), 0 4px 10px rgba(0,0,0,.1);
          display: inline-block; margin-left: 12px; vertical-align: middle;
        }
        .coin.spin { animation: flip 0.8s linear infinite; }
        @keyframes flip { from {transform: rotateY(0deg);} to {transform: rotateY(360deg);} }
      ")),
      # Keyboard shortcuts: F = flip, R = reset (pure Shiny setInputValue, no shinyjs)
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
                uiOutput("streakBar")
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

server <- function(input, output, session) {
  
  # --- State ---
  rv <- reactiveValues(
    points = 0,
    headChance = 0.20,
    flipDelay = 2.0,
    reward = 1,
    streakMultiplier = 1.0,
    flipping = FALSE,
    lastResult = "",
    log_df = NULL,         # data.frame(time, result, gained, streak, total_points, flip, chance)
    streak = 0,
    bestStreak = 0,
    won = FALSE,
    flipCount = 0,
    startTime = NULL,
    timeElapsed = 0,
    cost_increase_heads = 1,
    cost_faster_flip = 2,
    cost_extra_points = 3,
    cost_streak_multiplier = 3
  )
  
  # Helpers
  add_log <- function(result, gained) {
    row <- data.frame(
      time = format(Sys.time(), "%H:%M:%S"),
      result = result,
      gained = as.integer(gained),
      streak = rv$streak,
      total_points = rv$points,
      flip = rv$flipCount,
      chance = paste0(round(rv$headChance*100, 1), "%"),
      stringsAsFactors = FALSE
    )
    if (is.null(rv$log_df)) rv$log_df <- row else rv$log_df <- rbind(rv$log_df, row)
    # Keep last 200 rows
    if (nrow(rv$log_df) > 200) rv$log_df <- tail(rv$log_df, 200)
  }
  
  reset_values <- function() {
    rv$points <- 0; rv$headChance <- 0.20; rv$flipDelay <- 2.0; rv$reward <- 1; rv$streakMultiplier <- 1.0
    rv$flipping <- FALSE; rv$lastResult <- ""; rv$streak <- 0; rv$won <- FALSE
    rv$flipCount <- 0; rv$startTime <- NULL; rv$timeElapsed <- 0
    rv$cost_increase_heads <- 1; rv$cost_faster_flip <- 2; rv$cost_extra_points <- 3; rv$cost_streak_multiplier <- 3
    rv$log_df <- NULL
    rv$bestStreak <- 0
  }
  
  # ---- Current Status (header) ----
  output$currentStatus <- renderUI({
    tags$div(sprintf("Streak: %d | Points: %d", rv$streak, rv$points))
  })
  
  # ---- Value boxes ----
  output$vbPoints <- renderValueBox({
    valueBox(value = rv$points, subtitle = "Points", icon = icon("coins"), color = "yellow")
  })
  output$vbStreak <- renderValueBox({
    valueBox(value = rv$streak, subtitle = "Current Streak", icon = icon("fire"), color = "green")
  })
  output$vbBestStreak <- renderValueBox({
    valueBox(value = rv$bestStreak, subtitle = "Best Streak", icon = icon("trophy"), color = "teal")
  })
  output$vbHeadChance <- renderValueBox({
    valueBox(value = paste0(round(rv$headChance*100, 1), "%"), subtitle = "Head Chance", icon = icon("percentage"), color = "blue")
  })
  output$vbFlipDelay <- renderValueBox({
    valueBox(value = paste0(round(rv$flipDelay, 1), " s"), subtitle = "Flip Delay", icon = icon("stopwatch"), color = "purple")
  })
  
  # ---- Timer ----
  observe({
    invalidateLater(1000, session)
    if (!rv$won && !is.null(rv$startTime)) {
      rv$timeElapsed <- as.numeric(difftime(Sys.time(), rv$startTime, units = "secs"))
    }
  })
  
  # ---- Flip button ----
  observeEvent(input$flip, {
    if (rv$flipping || rv$won) return()
    rv$flipping <- TRUE
    
    # Disable buttons via JS (no shinyjs, pure DOM)
    session$sendCustomMessage("disableButtons", list(ids = c("flip", "upgradeLuck", "upgradeSpeed", "upgradeReward", "upgradeMultiplier")))
    
    # Start spin animation (no shinyjs)
    session$sendCustomMessage("coinSpin", list(spin = TRUE))
    
    rv$lastResult <- "Flipping..."
    output$result <- renderText(rv$lastResult)
    if (is.null(rv$startTime)) rv$startTime <- Sys.time()
    
    later(function() {
      flip <- runif(1)
      rv$flipCount <- rv$flipCount + 1
      
      if (flip < rv$headChance) {
        rv$streak <- rv$streak + 1
        rv$bestStreak <- max(rv$bestStreak, rv$streak)
        gained_points <- round(rv$reward * rv$streakMultiplier * rv$streak)
        rv$points <- rv$points + gained_points
        rv$lastResult <- paste0("Heads! +", gained_points, " points")
        add_log("Heads", gained_points)
      } else {
        rv$lastResult <- "Tails!"
        add_log("Tails", 0)
        rv$streak <- 0
      }
      
      # Win condition
      if (rv$streak >= 10 && !rv$won) {
        rv$won <- TRUE
        total_time <- round(as.numeric(difftime(Sys.time(), rv$startTime, units = "secs")), 1)
        rv$lastResult <- paste0("🎉 You win! 10 heads in a row! It took ", rv$flipCount, " flips and ", total_time, " seconds.")
        showNotification("🎉 Victory! You reached 10 heads in a row!", type = "message", duration = 6)
        showModal(modalDialog(
          title = "You win! 🏆",
          rv$lastResult,
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
      
      output$result <- renderText(rv$lastResult)
      
      # Streak progress bar (Bootstrap)
      output$streakBar <- renderUI({
        pct <- min(100, round(rv$streak / 10 * 100))
        cls <- if (rv$streak <= 3) "progress-bar progress-bar-danger"
        else if (rv$streak <= 6) "progress-bar progress-bar-warning"
        else "progress-bar progress-bar-success"
        div(class = "progress",
            div(class = cls, role = "progressbar",
                style = paste0("width:", pct, "%;"),
                paste0(rv$streak, " / 10")
            )
        )
      })
      
      rv$flipping <- FALSE
      
      # Stop spin and re-enable buttons (except when won)
      session$sendCustomMessage("coinSpin", list(spin = FALSE))
      session$sendCustomMessage("enableButtons", list(
        ids = if (rv$won) c() else c("flip", "upgradeLuck", "upgradeSpeed", "upgradeReward", "upgradeMultiplier")
      ))
    }, delay = rv$flipDelay)
  })
  
  # ---- Upgrades UI ----
  output$upgradeUI <- renderUI({
    tagList(
      # Head Chance
      actionButton("upgradeLuck", "⭐ Increase Head Chance (+5%)",
                   class = "btn btn-success btn-upg"),
      div(class = "cost", paste("Cost:", rv$cost_increase_heads, "pts")),
      br(),
      
      # Flip Speed
      actionButton("upgradeSpeed", "⚡ Decrease Flip Time (-0.2s)",
                   class = "btn btn-primary btn-upg"),
      div(class = "cost", paste("Cost:", rv$cost_faster_flip, "pts")),
      br(),
      
      # Reward
      actionButton("upgradeReward", "💰 Increase Reward (+1)",
                   class = "btn btn-warning btn-upg"),
      div(class = "cost", paste("Cost:", rv$cost_extra_points, "pts")),
      br(),
      
      # Multiplier
      actionButton("upgradeMultiplier", "📈 Streak Multiplier (+0.2×)",
                   class = "btn btn-info btn-upg"),
      div(class = "cost", paste("Cost:", rv$cost_streak_multiplier, "pts"))
    )
  })
  
  # Enable/disable upgrades (pure JS below handles the visual disabling too)
  observe({
    if (rv$won || rv$flipping) {
      session$sendCustomMessage("disableButtons", list(ids = c("upgradeLuck","upgradeSpeed","upgradeReward","upgradeMultiplier")))
    } else {
      # Disable only if not enough points
      ids_to_disable <- c()
      if (rv$points < rv$cost_increase_heads) ids_to_disable <- c(ids_to_disable, "upgradeLuck")
      if (rv$points < rv$cost_faster_flip) ids_to_disable <- c(ids_to_disable, "upgradeSpeed")
      if (rv$points < rv$cost_extra_points) ids_to_disable <- c(ids_to_disable, "upgradeReward")
      if (rv$points < rv$cost_streak_multiplier) ids_to_disable <- c(ids_to_disable, "upgradeMultiplier")
      
      session$sendCustomMessage("enableButtons", list(ids = setdiff(c("upgradeLuck","upgradeSpeed","upgradeReward","upgradeMultiplier"), ids_to_disable)))
      session$sendCustomMessage("disableButtons", list(ids = ids_to_disable))
    }
  })
  
  # ---- Upgrade handlers ----
  observeEvent(input$upgradeLuck, {
    if (rv$points >= rv$cost_increase_heads && !rv$won) {
      rv$points <- rv$points - rv$cost_increase_heads
      rv$headChance <- min(rv$headChance + 0.05, 0.99)
      rv$cost_increase_heads <- rv$cost_increase_heads * 2
      rv$streak <- 0
      output$streakBar <- renderUI({
        div(class = "progress",
            div(class = "progress-bar progress-bar-danger", role = "progressbar",
                style = "width:0%;", "0 / 10"))
      })
      showNotification("Luck upgraded: +5% head chance. Streak reset.", type = "message")
    }
  })
  
  observeEvent(input$upgradeSpeed, {
    if (rv$points >= rv$cost_faster_flip && !rv$won) {
      rv$points <- rv$points - rv$cost_faster_flip
      rv$flipDelay <- max(rv$flipDelay - 0.2, 0.2)
      rv$cost_faster_flip <- rv$cost_faster_flip * 2
      rv$streak <- 0
      output$streakBar <- renderUI({
        div(class = "progress",
            div(class = "progress-bar progress-bar-danger", role = "progressbar",
                style = "width:0%;", "0 / 10"))
      })
      showNotification("Speed upgraded: -0.2s flip time. Streak reset.", type = "message")
    }
  })
  
  observeEvent(input$upgradeReward, {
    if (rv$points >= rv$cost_extra_points && !rv$won) {
      rv$points <- rv$points - rv$cost_extra_points
      rv$reward <- rv$reward + 1
      rv$cost_extra_points <- rv$cost_extra_points * 2
      rv$streak <- 0
      output$streakBar <- renderUI({
        div(class = "progress",
            div(class = "progress-bar progress-bar-danger", role = "progressbar",
                style = "width:0%;", "0 / 10"))
      })
      showNotification("Reward upgraded: +1 point per head. Streak reset.", type = "message")
    }
  })
  
  observeEvent(input$upgradeMultiplier, {
    if (rv$points >= rv$cost_streak_multiplier && !rv$won) {
      rv$points <- rv$points - rv$cost_streak_multiplier
      rv$streakMultiplier <- round(rv$streakMultiplier + 0.2, 1)
      rv$cost_streak_multiplier <- rv$cost_streak_multiplier * 2
      rv$streak <- 0
      output$streakBar <- renderUI({
        div(class = "progress",
            div(class = "progress-bar progress-bar-danger", role = "progressbar",
                style = "width:0%;", "0 / 10"))
      })
      showNotification("Multiplier upgraded: +0.2×. Streak reset.", type = "message")
    }
  })
  
  # ---- Reset (with confirmation) ----
  observeEvent(input$reset, {
    showModal(modalDialog(
      title = "Reset game?",
      "This will reset your points, upgrades, streaks and log.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmReset", "Reset", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  observeEvent(input$confirmReset, {
    removeModal()
    reset_values()
    showNotification("Game reset.", type = "warning")
    
    # reset streak bar
    output$streakBar <- renderUI({
      div(class = "progress",
          div(class = "progress-bar progress-bar-danger", role = "progressbar",
              style = "width:0%;", "0 / 10"))
    })
  })
  
  # ---- Initial streak bar ----
  output$streakBar <- renderUI({
    div(class = "progress",
        div(class = "progress-bar progress-bar-danger", role = "progressbar",
            style = "width:0%;", "0 / 10"))
  })
  
  # ---- Stats table ----
  output$statsTable <- renderTable({
    data.frame(
      Metric = c("Points", "Head Chance", "Flip Delay", "Reward per Head", "Streak Multiplier", "Flips Made", "Time Elapsed"),
      Value = c(
        rv$points,
        paste0(round(rv$headChance * 100, 1), "%"),
        paste0(round(rv$flipDelay, 1), " sec"),
        rv$reward,
        paste0("×", round(rv$streakMultiplier, 1)),
        rv$flipCount,
        paste0(round(rv$timeElapsed, 0), " sec")
      ),
      stringsAsFactors = FALSE
    )
  }, colnames = FALSE)
  
  # ---- Flip log (DT) ----
  output$logDT <- renderDT({
    df <- rv$log_df
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No flips yet."), options = list(dom = 't'), rownames = FALSE))
    }
    datatable(
      df[seq(nrow(df),1), ],  # latest first
      options = list(pageLength = 5, lengthChange = FALSE, order = list(list(0, 'desc'))),
      rownames = FALSE
    )
  })
}

# --- Client-side helpers for enabling/disabling buttons & coin spin (no shinyjs) ---
js_disable_enable <- "
Shiny.addCustomMessageHandler('disableButtons', function(msg){
  (msg.ids || []).forEach(function(id){
    var el = document.getElementById(id);
    if(el){ el.setAttribute('disabled', 'disabled'); }
  });
});
Shiny.addCustomMessageHandler('enableButtons', function(msg){
  (msg.ids || []).forEach(function(id){
    var el = document.getElementById(id);
    if(el){ el.removeAttribute('disabled'); }
  });
});
Shiny.addCustomMessageHandler('coinSpin', function(msg){
  var el = document.getElementById('coin');
  if(!el) return;
  if(msg.spin){ el.classList.add('spin'); } else { el.classList.remove('spin'); }
});
"
# Inject the JS at app start
enable_js <- function() {
  insertUI("head", "beforeEnd", ui = tags$script(HTML(js_disable_enable)))
}
enable_js()

shinyApp(ui, server)