inTeams<-read.csv("Documents/R/Alistair/TeamEstim.csv")
head(inTeams)

mean.off<-mean(inTeams$off)
mean.def<-mean(inTeams$def)
lmean.off<-mean(log(inTeams$off))
lmean.def<-mean(log(inTeams$def) ) 
mean.off
mean.def

premLeague<-subset(inTeams,league=="Barclays Premier League")
premTeams<-premLeague$name
rownames(premLeague)<-premTeams
df.prem<-premLeague[,c("off","def")]
df.prem

lmean.def<- log(mean(df.prem$def))
lmean.off<- log(mean(df.prem$off))               
df.prem["alpha"]<-log(df.prem["off"])-lmean.def
df.prem["delta"]<-lmean.off-log(df.prem["def"])
head(df.prem) 
alphaList<-df.prem$alpha
deltaList<-df.prem$delta
names(alphaList)<-rownames(df.prem)
names(deltaList)<-rownames(df.prem)
alphaList["Liverpool"]

rpois(1,exp(alphaList["Liverpool"]-deltaList["Manchester City"]) )
c(rpois(1,exp(alphaList["Liverpool"]-deltaList["Manchester City"])),
  rpois(1,exp(alphaList["Manchester City"]-deltaList["Liverpool"])))
draw.score<-function(team1,team2){
    c(
        rpois(1,exp(alphaList[team1]-deltaList[team2])),
  rpois(1,exp(alphaList[team2]-deltaList[team1]))
    )
}

draw.score("Liverpool","Arsenal")
df.prem[c("Liverpool","Arsenal"),]
draw.score("Liverpool","Arsenal")
#install.packages('gtools')
library('gtools')
# All possible matches in a season
allMatches<-permutations(20, 2, v=rownames(df.prem),repeats.allowed=FALSE)
colnames(allMatches)<-c("home","away")
head(allMatches,9)
length(allMatches)
# Example scores through the entire season
ScoresMatrix <- matrix(nrow=nrow(allMatches),  ncol=4)
for (ii in 1:nrow(allMatches)  ) {
     ScoresMatrix[ii,1:2]=allMatches[ii,]
     ScoresMatrix[ii,3:4]= draw.score(allMatches[ii,"home"],allMatches[ii,"away"] )  
}
colnames(ScoresMatrix)<-c("home.team","away.team","home.score","away.score")
head(ScoresMatrix)

# This is how Claude responded to the assignment

library(readr)
library(dplyr)
library(ggplot2)

# Read the data
data <- read_csv("/Users/Swain/Documents/R/Alistair/TeamEstim.csv")

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

# Simulate entire season
simulate_season <- function(team_data, num_simulations = 10000) {
  # Create all possible match combinations
  teams <- team_data$team
  matches <- expand.grid(home_team = teams, away_team = teams) %>%
    filter(home_team != away_team)
  
  # Function to run a single season simulation
  run_simulation <- function() {
    # Simulate all matches
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
    
    # Calculate team standings
    standings <- bind_rows(
      # Home team results
      season_results %>%
        group_by(home_team) %>%
        summarise(
          matches_played = n(),
          goals_for = sum(home_goals),
          goals_against = sum(away_goals),
          wins = sum(result == "home_win") * 3,
          draws = sum(result == "draw"),
          points = wins + draws,
          goal_difference = goals_for - goals_against
        ) %>%
        rename(team = home_team),
      
      # Away team results
      season_results %>%
        group_by(away_team) %>%
        summarise(
          matches_played = n(),
          goals_for = sum(away_goals),
          goals_against = sum(home_goals),
          wins = sum(result == "away_win") * 3,
          draws = sum(result == "draw"),
          points = wins + draws,
          goal_difference = goals_for - goals_against
        ) %>%
        rename(team = away_team)
    ) %>%
      group_by(team) %>%
      summarise(
        matches_played = sum(matches_played),
        goals_for = sum(goals_for),
        goals_against = sum(goals_against),
        points = sum(points),
        goal_difference = sum(goal_difference)
      ) %>%
      arrange(desc(points), desc(goal_difference), desc(goals_for)) %>%
      mutate(position = row_number()) %>%
      mutate(revenue = revenue_mapping[as.character(position)])
    
    return(standings)
  }
  
  # Run multiple simulations
  sim_results <- replicate(num_simulations, run_simulation(), simplify = FALSE)
  
  return(sim_results)
}

# Run simulations
set.seed(123)
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

# Print key results
print(expected_positions)


