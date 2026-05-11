# Lade benötigte R-Pakete
library(shiny)

# Binde die ausgelagerte Modell-Logik ein
source("BasicModel_v1.0.R")

# ==============================================================================
# SHINY UI (Frontend)
# ==============================================================================
ui <- fluidPage(
  
  titlePanel("Live Kooperations-Modell (Natives R)"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Dieses Modell aktualisiert sich live, ähnlich wie in NetLogo."),
      
      # Steuerungs-Buttons (Setup und Go)
      fluidRow(
        column(6, actionButton("setup_btn", "Setup", icon = icon("undo"), class = "btn-warning", style = "width: 100%;")),
        column(6, actionButton("play_btn", "Play / Pause", icon = icon("play"), class = "btn-success", style = "width: 100%;"))
      ),
      
      hr(),
      
      # Slider für die Parameter
      sliderInput("coop_prob", 
                  "Kooperationswahrscheinlichkeit:", 
                  min = 0.0, max = 1.0, value = 0.5, step = 0.05),
      
      sliderInput("speed_log", 
                  "Simulationsgeschwindigkeit (Log-Skala):", 
                  min = 0, max = 3, value = 1, step = 0.1),
      helpText(strong(textOutput("actual_speed_text"))),
      
      hr(),
      h4(textOutput("tick_display"))
    ),
    
    mainPanel(
      fluidRow(
        column(6, plotOutput("mapPlot", height = "500px")),
        column(6, plotOutput("giniPlot", height = "500px"))
      )
    )
  )
)

# ==============================================================================
# SHINY SERVER (Controller)
# ==============================================================================
server <- function(input, output, session) {
  
  # Lokaler State Manager (verbindet UI mit dem Modell)
  rv <- reactiveValues(
    playing = FALSE,
    num_agents = 100,
    step = 0, wealth = NULL, x = NULL, y = NULL, color = NULL, gini_history = NULL
  )
  
  # ----------------------------------------------------------------------------
  # SETUP LOGIK
  # ----------------------------------------------------------------------------
  observeEvent(input$setup_btn, {
    rv$playing <- FALSE
    updateActionButton(session, "play_btn", label = "Play", icon = icon("play"))
    
    # Ruft setup_model() aus der model.R auf
    init_state <- setup_model(rv$num_agents)
    
    rv$step <- init_state$step
    rv$wealth <- init_state$wealth
    rv$x <- init_state$x
    rv$y <- init_state$y
    rv$color <- init_state$color
    rv$gini_history <- init_state$gini_history
  }, ignoreNULL = FALSE) 
  
  # ----------------------------------------------------------------------------
  # PLAY / PAUSE LOGIK
  # ----------------------------------------------------------------------------
  observeEvent(input$play_btn, {
    rv$playing <- !rv$playing
    
    if (rv$playing) {
      updateActionButton(session, "play_btn", label = "Pause", icon = icon("pause"))
    } else {
      updateActionButton(session, "play_btn", label = "Play", icon = icon("play"))
    }
  })
  
  # ----------------------------------------------------------------------------
  # SIMULATIONS-SCHLEIFE
  # ----------------------------------------------------------------------------
  observe({
    req(rv$playing)
    invalidateLater(100, session)
    
    isolate({
      # Ruft die Schleife aus der model.R auf
      new_state <- run_model_steps(
        step = rv$step,
        wealth = rv$wealth,
        gini_history = rv$gini_history,
        speed = round(10^input$speed_log),
        prob = input$coop_prob,
        num_agents = rv$num_agents
      )
      
      # Neue Werte übernehmen
      rv$step <- new_state$step
      rv$wealth <- new_state$wealth
      rv$color <- new_state$color
      rv$gini_history <- new_state$gini_history
    })
  })
  
  # ----------------------------------------------------------------------------
  # OUTPUTS ZEICHNEN
  # ----------------------------------------------------------------------------
  output$actual_speed_text <- renderText({
    paste("Aktuelle Ticks pro Frame:", round(10^input$speed_log))
  })
  
  output$tick_display <- renderText({
    paste("Aktueller Tick:", rv$step)
  })
  
  output$mapPlot <- renderPlot({
    req(rv$step >= 0)
    par(mar = c(2, 2, 4, 1)) 
    plot(rv$x, rv$y, col = rv$color, pch = 19, cex = 2,
         xlim = c(-16, 16), ylim = c(-16, 16), asp = 1,
         main = "Räumliche Verteilung\n(Grün = Kooperierende im ZULETZT berechneten Tick)",
         xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE)
  })
  
  output$giniPlot <- renderPlot({
    req(rv$step > 0)
    step_seq <- 1:rv$step
    gini_vals <- rv$gini_history[1:rv$step]
    
    par(mar = c(4, 4, 4, 1))
    plot(step_seq, gini_vals, type = "l",
         col = "darkred", lwd = 2,
         ylim = c(0, max(0.1, max(gini_vals, na.rm = TRUE))),
         main = "Gini-Index über die Zeit",
         xlab = "Ticks", ylab = "Gini-Index")
  })
}

# Starte die App
shinyApp(ui = ui, server = server)