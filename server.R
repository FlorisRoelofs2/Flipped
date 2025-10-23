# server.R
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(later)

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
    updateProgressBar(session, "streakPB", value = 0, total = 10, status = "danger")
  }
  
  # ---- Current Status (header) ----
  output$currentStatus <- renderUI({
    tags$div(
      sprintf("Streak: %d | Points: %d", rv$streak, rv$points)
    )
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
    shinyjs::disable("flip")
    shinyjs::disable(selector = ".upgrade-btn")
    shinyjs::toggleClass(id = "coin", class = "spinning", add = TRUE)
    
    rv$lastResult <- "Flipping..."
    output$result <- renderText(rv$lastResult)
    if (is.null(rv$startTime)) rv$startTime <- Sys.time()
    
    # Delay to simulate flip time
    later(function() {
      isolate({
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
          sendSweetAlert(session, title = "You win!", text = rv$lastResult, type = "success")
        }
        
        # Update UI bits
        output$result <- renderText(rv$lastResult)
        
        # Streak progress bar color
        st <- rv$streak
        status <- if (st <= 3) "danger" else if (st <= 6) "warning" else "success"
        updateProgressBar(session, "streakPB", value = min(st, 10), total = 10, status = status)
        
        # Re-enable
        rv$flipping <- FALSE
        shinyjs::toggleClass(id = "coin", class = "spinning", add = FALSE)
        
        if (!rv$won) shinyjs::enable("flip")
      })
    }, delay = rv$flipDelay)
  })
  
  # ---- Upgrades UI ----
  output$upgradeUI <- renderUI({
    tags$div(
      actionBttn(
        inputId = "upgradeLuck",
        label = "⭐ Increase Head Chance (+5%)",
        style = "fill", color = "success", size = "sm", block = TRUE, class = "upgrade-btn"
      ),
      div(class = "text-muted",
          tags$span(class = "badge bg-green cost-badge", paste("Cost:", rv$cost_increase_heads, "pts"))
      ),
      br(),
      
      actionBttn(
        inputId = "upgradeSpeed",
        label = "⚡ Decrease Flip Time (-0.2s)",
        style = "fill", color = "primary", size = "sm", block = TRUE, class = "upgrade-btn"
      ),
      div(class = "text-muted",
          tags$span(class = "badge bg-blue cost-badge", paste("Cost:", rv$cost_faster_flip, "pts"))
      ),
      br(),
      
      actionBttn(
        inputId = "upgradeReward",
        label = "💰 Increase Reward (+1)",
        style = "fill", color = "warning", size = "sm", block = TRUE, class = "upgrade-btn"
      ),
      div(class = "text-muted",
          tags$span(class = "badge bg-yellow cost-badge", paste("Cost:", rv$cost_extra_points, "pts"))
      ),
      br(),
      
      actionBttn(
        inputId = "upgradeMultiplier",
        label = "📈 Streak Multiplier (+0.2×)",
        style = "fill", color = "purple", size = "sm", block = TRUE, class = "upgrade-btn"
      ),
      div(class = "text-muted",
          tags$span(class = "badge bg-purple cost-badge", paste("Cost:", rv$cost_streak_multiplier, "pts"))
      )
    )
  })
  
  # Enable/disable upgrades based on points / state
  observe({
    shinyjs::toggleState("upgradeLuck", condition = rv$points >= rv$cost_increase_heads && !rv$won && !rv$flipping)
    shinyjs::toggleState("upgradeSpeed", condition = rv$points >= rv$cost_faster_flip && !rv$won && !rv$flipping)
    shinyjs::toggleState("upgradeReward", condition = rv$points >= rv$cost_extra_points && !rv$won && !rv$flipping)
    shinyjs::toggleState("upgradeMultiplier", condition = rv$points >= rv$cost_streak_multiplier && !rv$won && !rv$flipping)
  })
  
  # ---- Upgrade handlers ----
  observeEvent(input$upgradeLuck, {
    if (rv$points >= rv$cost_increase_heads && !rv$won) {
      rv$points <- rv$points - rv$cost_increase_heads
      rv$headChance <- min(rv$headChance + 0.05, 0.99)
      rv$cost_increase_heads <- rv$cost_increase_heads * 2
      rv$streak <- 0
      updateProgressBar(session, "streakPB", value = 0, total = 10, status = "danger")
      showNotification("Luck upgraded: +5% head chance. Streak reset.", type = "message")
    }
  })
  observeEvent(input$upgradeSpeed, {
    if (rv$points >= rv$cost_faster_flip && !rv$won) {
      rv$points <- rv$points - rv$cost_faster_flip
      rv$flipDelay <- max(rv$flipDelay - 0.2, 0.2)
      rv$cost_faster_flip <- rv$cost_faster_flip * 2
      rv$streak <- 0
      updateProgressBar(session, "streakPB", value = 0, total = 10, status = "danger")
      showNotification("Speed upgraded: -0.2s flip time. Streak reset.", type = "message")
    }
  })
  observeEvent(input$upgradeReward, {
    if (rv$points >= rv$cost_extra_points && !rv$won) {
      rv$points <- rv$points - rv$cost_extra_points
      rv$reward <- rv$reward + 1
      rv$cost_extra_points <- rv$cost_extra_points * 2
      rv$streak <- 0
      updateProgressBar(session, "streakPB", value = 0, total = 10, status = "danger")
      showNotification("Reward upgraded: +1 point per head. Streak reset.", type = "message")
    }
  })
  observeEvent(input$upgradeMultiplier, {
    if (rv$points >= rv$cost_streak_multiplier && !rv$won) {
      rv$points <- rv$points - rv$cost_streak_multiplier
      rv$streakMultiplier <- round(rv$streakMultiplier + 0.2, 1)
      rv$cost_streak_multiplier <- rv$cost_streak_multiplier * 2
      rv$streak <- 0
      updateProgressBar(session, "streakPB", value = 0, total = 10, status = "danger")
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
    shinyjs::enable("flip")
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
