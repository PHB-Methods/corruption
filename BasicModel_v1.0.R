# ==============================================================================
# REINE MODELL-LOGIK (Kein UI-Code hier)
# ==============================================================================

# 1. Hilfsfunktion zur Berechnung des Gini-Index
calculate_gini <- function(wealths) {
  wealths <- sort(wealths)
  n <- length(wealths)
  total_wealth <- sum(wealths)
  
  if (total_wealth <= 0) return(0)
  
  weighted_sum <- sum(seq_len(n) * wealths)
  gini <- ((2 * weighted_sum) / (n * total_wealth)) - ((n + 1) / n)
  return(gini)
}

# 2. Setup-Funktion: Generiert den initialen Systemzustand
setup_model <- function(num_agents = 100) {
  wealth <- rep(100, num_agents)
  
  # Vorab-Allokation für hohe Performance
  gini_history <- rep(NA_real_, 100000)
  gini_history[1] <- calculate_gini(wealth)
  
  list(
    step = 0,
    wealth = wealth,
    x = runif(num_agents, -16, 16),
    y = runif(num_agents, -16, 16),
    color = rep("blue", num_agents),
    gini_history = gini_history
  )
}

# 3. Go-Funktion: Führt N Schritte der Simulation am Stück aus
run_model_steps <- function(step, wealth, gini_history, speed, prob, num_agents) {
  loss <- 2 / (num_agents - 2)
  color <- rep("blue", num_agents)
  
  for (i in seq_len(speed)) {
    step <- step + 1
    
    # Farbe für DIESEN spezifischen Tick zurücksetzen
    color <- rep("blue", num_agents)
    
    # Interaktion findet statt?
    if (runif(1) < prob) {
      # Ziehe zwei Agenten
      pair <- sample.int(num_agents, 2, replace = FALSE)
      
      # Gewinner aktualisieren und Farbe auf grün setzen
      wealth[pair] <- wealth[pair] + 1
      color[pair] <- "green"
      
      # Rest verliert Wohlstand
      wealth[-pair] <- wealth[-pair] - loss
    }
    
    # Falls wir über 100.000 Ticks kommen, den Vektor vergrößern
    if (step > length(gini_history)) {
      gini_history <- c(gini_history, rep(NA_real_, 100000))
    }
    
    # Gini berechnen
    gini_history[step] <- calculate_gini(wealth)
  }
  
  # Aktualisierten Zustand zurückgeben
  return(list(
    step = step,
    wealth = wealth,
    color = color,
    gini_history = gini_history
  ))
}