library(readr)
library(dplyr)
library(ggplot2)

# Read the data
team_data <- read.csv("/Users/Swain/Documents/R/Alistair/TeamEstim.csv")

# Revenue mapping
revenue_mapping <- c(
  "1" = 149.6, "2" = 145.9, "3" = 142.1, "4" = 138.4, "5" = 73.7, 
  "6" = 70.0, "7" = 55.2, "8" = 33.5, "9" = 29.8, "10" = 26.0, 
  "11" = 22.3, "12" = 18.6, "13" = 14.9, "14" = 11.2, "15" = 7.5, 
  "16" = 3.7, "17" = 0, "18" = -88.7, "19" = -92.5, "20" = -96.2
)

# Simulate a single match
simulate_match <- function(home_team, away_team, team_data) {
  # Extract team parameters
  home_alpha <- team_data$alpha[team_data$team == home_team]
  away_delta <- team_data$delta[team_data$team == away_team]
  
  # Calculate expected goals using log-linear model
  home_expected_goals <- exp(home_alpha - away_delta)
  away_alpha <- team_data$alpha[team_data$team == away_team]
  home_delta <- team_data$delta[team_data$team == home_team]
  away_expected_goals <- exp(away_alpha - home_delta)
  
  # Simulate goals using Poisson distribution
  home_goals <- rpois(1, home_expected_goals)
  away_goals <- rpois(1, away_expected_goals)
  
  # Return result
  list(
    home_team = home_team, 
    away_team = away_team, 
    home_goals = home_goals, 
    away_goals = away_goals
  )
}

simulate_match("Liverpool","Chelsea",team_data)

library(tidyr)

simulate_season <- function(team_data, num_simulations = 1000, revenue_mapping) {
  teams <- team_data$team
  matches <- expand.grid(home_team = teams, away_team = teams) %>%
    filter(home_team != away_team)
  
  simulation_results <- list()
  
  calculate_standings <- function(season_results, revenue_mapping = NULL) {
    standings <- bind_rows(
      season_results %>%
        group_by(home_team) %>%
        summarise(
          matches_played = n(),
          goals_for = sum(home_goals),
          goals_against = sum(away_goals),
          wins = sum(result == "home_win"),
          draws = sum(result == "draw"),
          losses = sum(result == "away_win"),
          points = wins * 3 + draws,
          goal_difference = goals_for - goals_against
        ) %>%
        rename(team = home_team),
      
      season_results %>%
        group_by(away_team) %>%
        summarise(
          matches_played = n(),
          goals_for = sum(away_goals),
          goals_against = sum(home_goals),
          wins = sum(result == "away_win"),
          draws = sum(result == "draw"),
          losses = sum(result == "home_win"),
          points = wins * 3 + draws,
          goal_difference = goals_for - goals_against
        ) %>%
        rename(team = away_team)
    ) %>%
      group_by(team) %>%
      summarise(
        matches_played = sum(matches_played) / 2,
        goals_for = sum(goals_for),
        goals_against = sum(goals_against),
        wins = sum(wins),
        draws = sum(draws),
        losses = sum(losses),
        points = sum(points),
        goal_difference = sum(goal_difference)
      ) %>%
      arrange(desc(points), desc(goal_difference), desc(goals_for)) %>%
      mutate(position = row_number())
    
    if (!is.null(revenue_mapping)) {
      standings <- standings %>%
        mutate(revenue = revenue_mapping[as.character(position)])
    }
    
    return(standings)
  }
  
  for (sim in 1:num_simulations) {
    season_results <- matches %>%
      rowwise() %>%
      mutate(match = list(simulate_match(home_team, away_team, team_data))) %>%
      mutate(
        home_goals = match$home_goals,
        away_goals = match$away_goals,
        result = case_when(
          home_goals > away_goals ~ "home_win",
          home_goals < away_goals ~ "away_win",
          TRUE ~ "draw"
        )
      ) %>%
      ungroup()
    
    simulation_standings <- calculate_standings(season_results, revenue_mapping)
    simulation_results[[sim]] <- simulation_standings
  }
  
  combined_results <- bind_rows(simulation_results, .id = "simulation") %>%
    group_by(team) %>%
    summarise(
      avg_position = mean(position),
      position_std = sd(position),
      avg_points = mean(points),
      points_std = sd(points),
      avg_goals_for = mean(goals_for),
      goals_for_std = sd(goals_for),
      avg_goals_against = mean(goals_against),
      goals_against_std = sd(goals_against),
      avg_revenue = mean(revenue, na.rm = TRUE),
    revenue_std = sd(revenue, na.rm = TRUE)
    ) %>%
    arrange(avg_position)
  
  return(combined_results)
}

# Simulate the season
season_simulation <- simulate_season(team_data, num_simulations = 1000,revenue_mapping)
print(season_simulation)
# Cache simulation
season_simulation <- write.csv(season_simulation, "season_simulation.csv")

# TO DO - RANDOM TIEBREAKER NUMBER

# QUESTIONS BELOW - FIX GRAPH VARIABLES

# Assemble a ranking of the teams from best to worst according to their expected position in the table.
    # Illustrate this ranking with a clear visualization of the expected position

season_simulations <- simulate_season(data)

# Expected Position Analysis
expected_positions <- do.call(rbind, season_simulations) %>%
  group_by(team) %>%
  summarise(
    mean_position = mean(position),
    position_std = sd(position),
    mean_revenue = mean(revenue)
  ) %>%
  arrange(mean_position)

# Visualization of Expected Positions
ggplot(expected_positions, aes(x = reorder(team, -mean_position), y = mean_position)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_position - position_std, 
                    ymax = mean_position + position_std), 
                width = 0.2) +
  coord_flip() +
  labs(title = "Expected League Positions with Variability",
       x = "Team", y = "Expected Position") +
  theme_minimal()

# Which teams have the most variability in their simulated earnings?
    # Again, illustrate this variability with a clear visualization.

# Earnings Variability Visualization
ggplot(expected_positions, aes(x = reorder(team, mean_revenue), y = mean_revenue)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_errorbar(aes(ymin = mean_revenue - position_std, 
                    ymax = mean_revenue + position_std), 
                width = 0.2) +
  coord_flip() +
  labs(title = "Team Earnings Variability",
       x = "Team", y = "Expected Revenue (£ millions)") +
  theme_minimal()

# Which teams stand to benefit the most in monetary terms from a "lucky win" (converting one of their simulated losses to a win, holding everyone else constant)?
    # Again provide a visualization of this marginal effect
# Finally, for each team, calculate the monetary benefit to each team from either (i) an increase of 10% to their expected goals scored (holding everyone else constant), or (ii) a decreasing in their expected goals conceded by 10% (Note that this means 40 separate simulations!)
    # Use this to provide a clear visualization of the benefits of investing in offense versus defense by team

