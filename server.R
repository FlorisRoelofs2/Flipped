server <- function(input, output, session) {
  rv <- reactiveValues(
    points = 0,
    headChance = 0.20,
    flipDelay = 2.0,
    reward = 1,
    streakMultiplier = 1.0,
    flipping = FALSE,
    lastResult = "",
    log = character(),
    streak = 0,
    won = FALSE,
    flipCount = 0,
    startTime = NULL,
    timeElapsed = 0,
    cost_increase_heads = 1,
    cost_faster_flip = 2,
    cost_extra_points = 3,
    cost_streak_multiplier = 3
  )
  
  # ---- Current Status ----
  output$currentStatus <- renderUI({
    tags$div(paste0("Streak: ", rv$streak, " | Points: ", rv$points))
  })
  
  # ---- Upgrades UI ----
  output$upgradeUI <- renderUI({
    tagList(
      fluidRow(
        column(12, actionButton("upgradeLuck", "⭐ Increase Head Chance (+5%)", style = "color: #fff; background-color: #4CAF50; border-color: #388E3C")),
        column(12, textOutput("costLuck")),
        column(12, actionButton("upgradeSpeed", "⚡ Decrease Flip Time (-0.2s)", style = "color: #fff; background-color: #2196F3; border-color: #1976D2")),
        column(12, textOutput("costSpeed")),
        column(12, actionButton("upgradeReward", "💰 Increase Reward (+1)", style = "color: #fff; background-color: #FF9800; border-color: #F57C00")),
        column(12, textOutput("costReward")),
        column(12, actionButton("upgradeMultiplier", "📈 Streak Multiplier (+0.2×)", style = "color: #fff; background-color: #9C27B0; border-color: #7B1FA2")),
        column(12, textOutput("costMultiplier"))
      )
    )
  })
  
  output$costLuck <- renderText({ paste("Cost:", rv$cost_increase_heads, "pts") })
  output$costSpeed <- renderText({ paste("Cost:", rv$cost_faster_flip, "pts") })
  output$costReward <- renderText({ paste("Cost:", rv$cost_extra_points, "pts") })
  output$costMultiplier <- renderText({ paste("Cost:", rv$cost_streak_multiplier, "pts") })
  
  # Timer
  observe({ invalidateLater(1000, session); 
    if (!rv$won && !is.null(rv$startTime)) rv$timeElapsed <- as.numeric(difftime(Sys.time(), rv$startTime, units="secs")) 
  })
  
  # Flip coin
  observeEvent(input$flip, {
    if (rv$flipping || rv$won) return()
    rv$flipping <- TRUE
    rv$lastResult <- "Flipping..."
    output$result <- renderText(rv$lastResult)
    
    if (is.null(rv$startTime)) rv$startTime <- Sys.time()
    
    later(function() {
      isolate({
        flip <- runif(1)
        rv$flipCount <- rv$flipCount + 1
        
        if (flip < rv$headChance) {
          rv$streak <- rv$streak + 1
          gained_points <- round(rv$reward * rv$streakMultiplier * rv$streak)
          rv$points <- rv$points + gained_points
          rv$lastResult <- paste0("Heads! +", gained_points, " points")
        } else {
          rv$lastResult <- "Tails!"
          rv$streak <- 0
        }
        
        rv$log <- c(rv$log, rv$lastResult)
        if (length(rv$log) > 50) rv$log <- tail(rv$log, 50)
        
        if (rv$streak >= 10) {
          rv$won <- TRUE
          total_time <- round(as.numeric(difftime(Sys.time(), rv$startTime, units="secs")), 1)
          rv$lastResult <- paste0("🎉 You win! 10 heads in a row! It took ", rv$flipCount, " flips and ", total_time, " seconds.")
        }
        
        rv$flipping <- FALSE
        output$result <- renderText(rv$lastResult)
      })
    }, delay = rv$flipDelay)
  })
  
  # Upgrades
  observeEvent(input$upgradeLuck, { 
    if (rv$points >= rv$cost_increase_heads && !rv$won) {
      rv$points <- rv$points - rv$cost_increase_heads
      rv$headChance <- min(rv$headChance + 0.05, 0.99)
      rv$cost_increase_heads <- rv$cost_increase_heads * 2
      rv$streak <- 0
    }
  })
  observeEvent(input$upgradeSpeed, { 
    if (rv$points >= rv$cost_faster_flip && !rv$won) {
      rv$points <- rv$points - rv$cost_faster_flip
      rv$flipDelay <- max(rv$flipDelay - 0.2, 0.2)
      rv$cost_faster_flip <- rv$cost_faster_flip * 2
      rv$streak <- 0
    }
  })
  observeEvent(input$upgradeReward, { 
    if (rv$points >= rv$cost_extra_points && !rv$won) {
      rv$points <- rv$points - rv$cost_extra_points
      rv$reward <- rv$reward + 1
      rv$cost_extra_points <- rv$cost_extra_points * 2
      rv$streak <- 0
    }
  })
  observeEvent(input$upgradeMultiplier, { 
    if (rv$points >= rv$cost_streak_multiplier && !rv$won) {
      rv$points <- rv$points - rv$cost_streak_multiplier
      rv$streakMultiplier <- round(rv$streakMultiplier + 0.2, 1)
      rv$cost_streak_multiplier <- rv$cost_streak_multiplier * 2
      rv$streak <- 0
    }
  })
  
  # Reset
  observeEvent(input$reset, {
    rv$points <- 0; rv$headChance <- 0.20; rv$flipDelay <- 2.0; rv$reward <- 1; rv$streakMultiplier <- 1.0
    rv$flipping <- FALSE; rv$lastResult <- ""; rv$log <- character(); rv$streak <- 0; rv$won <- FALSE
    rv$flipCount <- 0; rv$startTime <- NULL; rv$timeElapsed <- 0
    rv$cost_increase_heads <- 1; rv$cost_faster_flip <- 2; rv$cost_extra_points <- 3; rv$cost_streak_multiplier <- 3
  })
  
  # Stats
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
      )
    )
  }, colnames = FALSE)
  
  # Flip log
  output$log <- renderPrint({
    if (length(rv$log) == 0) return("No flips yet.")
    cat(rev(rv$log), sep = "\n")
  })
  
  # Streak bar
  output$streakBar <- renderUI({
    div(class = "progress",
        div(class = "progress-bar bg-success", role = "progressbar",
            style = paste0("width:", (rv$streak / 10) * 100, "%"),
            paste0(rv$streak, " / 10")
        )
    )
  })
}
