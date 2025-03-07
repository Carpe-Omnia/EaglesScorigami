library(dplyr)
library(ggplot2)
library(plotly)

# Load data
myGames <- readRDS("./data/games.rds")
eaglesGames <- subset(myGames, home_team == "PHI" | away_team == "PHI")

# Prepare data
myEaglesScores <- data.frame(
  eagles_score = numeric(),
  lame_team_score = numeric(),
  lame_team_name = character(),
  eagles_win = character(),
  date = character()
)

for (i in 1:nrow(eaglesGames)) {
  if (eaglesGames[i, 10] == "PHI") {
    myEaglesScores[i, 1] <- eaglesGames[i, 11] # eagles/home team score
    myEaglesScores[i, 2] <- eaglesGames[i, 9] # away team score
    myEaglesScores[i, 3] <- eaglesGames[i, 8] # away team name
  } else {
    myEaglesScores[i, 1] <- eaglesGames[i, 9] # away/eagles team score
    myEaglesScores[i, 2] <- eaglesGames[i, 11] # lameTeam/home team score
    myEaglesScores[i, 3] <- eaglesGames[i, 10] # lameTeam/home team name
  }
  if (myEaglesScores[i, 1] > myEaglesScores[i, 2]) {
    myEaglesScores[i, 4] <- "Win" # eagles win
  } else if (myEaglesScores[i, 1] == myEaglesScores[i, 2]) {
    myEaglesScores[i, 4] <- "Tie" # eagles tie
  } else {
    myEaglesScores[i, 4] <- "Loss" # eagles lose
  }
  myEaglesScores[i, 5] <- eaglesGames[i, 5] # date of the game
}

# Group games by score
grouped_scores <- myEaglesScores %>%
  group_by(eagles_score, lame_team_score) %>%
  summarize(
    games = paste0(
      "Date: ", date, " | Lame Team: ", lame_team_name, 
      " | Birds Score: ", eagles_score, " | Lame Team Score: ", lame_team_score,
      collapse = "<br>"
    ),
    eagles_win = first(eagles_win), # Use the first outcome for coloring
    .groups = "drop"
  )

# Create a plotly object
p <- plot_ly()

# Define colors for each outcome
outcome_colors <- c("Win" = "#004953", "Loss" = "green", "Tie" = "black")

# Add rectangles for each group of games
shapes_list <- list()
for (i in 1:nrow(grouped_scores)) {
  shapes_list[[i]] <- list(
    type = "rect",
    x0 = grouped_scores$lame_team_score[i],
    x1 = grouped_scores$lame_team_score[i] + 1,
    y0 = grouped_scores$eagles_score[i],
    y1 = grouped_scores$eagles_score[i] + 1,
    fillcolor = outcome_colors[grouped_scores$eagles_win[i]],
    line = list(color = "gray", width = 0.5)
  )
  
  # Add hover text for each group of games
  p <- p %>% add_annotations(
    x = grouped_scores$lame_team_score[i] + 0.5,
    y = grouped_scores$eagles_score[i] + 0.5,
    text = "", # No visible text
    hovertext = grouped_scores$games[i], # Show all games with this score
    hoverinfo = "text",
    showarrow = FALSE,
    hoverlabel = list(
      namelength = -1,
      bgcolor = outcome_colors[grouped_scores$eagles_win[i]] # Match hover background to outcome color
    )
  )
}

# Add gridlines
gridlines <- list()
for (i in 0:60) {
  gridlines <- c(gridlines, list(
    list(type = "line", x0 = i, x1 = i, y0 = 0, y1 = 60, 
         line = list(color = ifelse(i %% 5 == 0, "black", "gray"), width = ifelse(i %% 5 == 0, 1, 0.5))),
    list(type = "line", x0 = 0, x1 = 60, y0 = i, y1 = i, 
         line = list(color = ifelse(i %% 5 == 0, "black", "gray"), width = ifelse(i %% 5 == 0, 1, 0.5)))
  ))
}

# Combine shapes and gridlines
all_shapes <- c(shapes_list, gridlines)

# Define tick values and labels for every 5 units
tickvals <- seq(0, 60, by = 5)
ticktext <- as.character(tickvals)

# Final layout
p <- p %>% layout(
  xaxis = list(
    range = c(0, 60), 
    dtick = 1, 
    tick0 = 0, 
    tickmode = "array", 
    ticksuffix = " ", 
    title = "Lame Team Score",
    ticktext = ticktext,
    tickvals = tickvals
  ),
  yaxis = list(
    range = c(0, 60), 
    dtick = 1, 
    tick0 = 0, 
    tickmode = "array", 
    ticksuffix = " ", 
    title = "Eagles Score",
    ticktext = ticktext,
    tickvals = tickvals
  ),
  shapes = all_shapes,
  title = "Eagles Scorigami",
  showlegend = FALSE, # No legend
  yaxis = list(scaleanchor = "x", scaleratio = 1),
  autosize = FALSE, 
  width = 600, 
  height = 600
)

p