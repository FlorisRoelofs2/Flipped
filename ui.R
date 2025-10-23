# ui.R

ui <- fluidPage(
  theme = app_theme,
  
  titlePanel("🎲 Unfair Flips"),
  br(),
  
  tabsetPanel(
    # Tab 1: Introductie
    tabPanel(
      "Introduction",
      br(),
      h3("Welcome to Unfair Flips!"),
      p("In this game, your goal is simple but challenging: flip a coin and try to get 10 heads in a row."),
      
      h4("How it Works:"),
      tags$ul(
        tags$li("Click 'Flip Coin' to flip the coin."),
        tags$li("Each heads adds points based on your current reward and streak multiplier."),
        tags$li("Tails resets your current streak back to zero."),
        tags$li("Try to get 10 heads in a row to win!")
      ),
      
      h4("Upgrades and Strategy:"),
      p("You can spend points earned from flipping heads to buy upgrades:"),
      tags$ul(
        tags$li("⭐ Increase Head Chance: Makes landing heads more likely (+5%). Each upgrade purchase increases the cost for the next upgrade."),
        tags$li("⚡ Decrease Flip Time: Shorter delay between flips (-0.2 seconds). You cannot reduce the flip delay below 0.2 seconds."),
        tags$li("💰 Increase Reward per Head: Each head gives more points (+1 per upgrade)."),
        tags$li("📈 Streak Multiplier: Your points for consecutive heads are multiplied. For example, if your streak is 3 and multiplier is 1.2×, points = reward × 3 × 1.2."),
        tags$li("⚠️ Buying an upgrade resets your current streak, so plan strategically!")
      ),
      
      h4("Challenge Yourself:"),
      tags$ul(
        tags$li("Upgrade costs increase each time you buy an upgrade, making strategic planning important."),
        tags$li("Try to beat the game in fewer than 300 flips or under 500 seconds for a personal high score."),
        tags$li("Keep an eye on your streak progress at the top. The longer your streak, the more points you earn!")
      ),
      
      br(),
      p("Once you feel ready, switch to the 'Play' tab to start flipping and put your strategy to the test!")
    ),
    
    # Tab 2: Play
    tabPanel(
      "Play",
      fluidRow(
        column(
          6,
          actionButton("flip", "Flip Coin", class = "btn-primary btn-lg"),
          actionButton("reset", "Reset Game", class = "btn-danger btn-sm", style = "margin-left:10px;"),
          br(), br(),
          h4("Result"),
          textOutput("result"),
          br(),
          h4("Streak Progress"),
          uiOutput("streakBar"),
          br(),
          h4("Stats"),
          tableOutput("statsTable"),
          h4("Flip Log"),
          tags$div(
            style = "height:150px; overflow-y:scroll; background:#f8f9fa; padding:10px; border:1px solid #ddd;",
            verbatimTextOutput("log")
          )
        ),
        column(
          6,
          h4("Upgrades"),
          tagList(
            lapply(
              list(
                list(id = "upgradeLuck", icon = "star", label = "Increase Head Chance (+5%)", btnClass = "btn-success", cost = "costLuck"),
                list(id = "upgradeSpeed", icon = "bolt", label = "Decrease Flip Time (-0.2s)", btnClass = "btn-info", cost = "costSpeed"),
                list(id = "upgradeReward", icon = "coins", label = "Increase Reward per Head (+1 point)", btnClass = "btn-warning", cost = "costReward"),
                list(id = "upgradeMultiplier", icon = "chart-line", label = "Increase Streak Multiplier (+0.2×)", btnClass = "btn-secondary", cost = "costMultiplier")
              ),
              function(upg) {
                fluidRow(
                  column(8, actionButton(upg$id, tagList(icon(upg$icon), upg$label), class = upg$btnClass)),
                  column(4, textOutput(upg$cost))
                )
              }
            )
          )
        )
      )
    )
  )
)
