# Building the Data Set

# Load Necessary Packages
library(rvest)
library(tidyverse)
library(janitor)
library(readr)

################################################################################

# Web Scraping Advanced Statistics

url <- ""
advancedStats <- tibble()

for(i in seq(2000, 2024)){
  url <- paste0(
    "https://www.basketball-reference.com/leagues/NBA_",
    i,
    ".html#all_confs_standings_E"
  )
  
  web_data <- 
    read_html(url) |>
    html_nodes(xpath = '//*[@id="advanced-team"]') |>
    html_table()
  
  web_df <- web_data[[1]]
  
  web_df_clean <-
    web_df |>
      janitor::row_to_names(row_number = 1) |>
      janitor::clean_names(case = 'none') |>
      mutate(Season = i) |>
      filter(Team != "Team")
  
  advancedStats <- bind_rows(advancedStats, web_df_clean)
}

# Cleaning the Data
advancedStats <- advancedStats |> 
  # Rename some attributes
  rename(
    Def_eFG_percent = eFG_percent_2,
    Def_TOV_percent = TOV_percent_2,
    Def_FT_FGA = FT_FGA_2
  ) |>
  # Remove unnecessary attributes
  select(-Rk, -Arena, -Attend, -Attend_G, -`NA`, -NA_2, -NA_3) |>
  # Type Casting
  mutate(
    Age = as.numeric(Age),
    W = as.numeric(W),
    L = as.numeric(L),
    PW = as.numeric(PW),
    PL = as.numeric(PL),
    MOV = as.numeric(MOV),
    SOS = as.numeric(SOS),
    SRS = as.numeric(SRS),
    ORtg = as.numeric(ORtg),
    DRtg = as.numeric(DRtg),
    NRtg = as.numeric(NRtg),
    Pace = as.numeric(Pace),
    FTr = as.numeric(FTr),
    X3PAr = as.numeric(X3PAr),
    TS_percent = as.numeric(TS_percent),
    eFG_percent = as.numeric(eFG_percent),
    TOV_percent = as.numeric(TOV_percent),
    ORB_percent = as.numeric(ORB_percent),
    FT_FGA = as.numeric(FT_FGA),
    DRB_percent = as.numeric(DRB_percent),
    Def_eFG_percent = as.numeric(Def_eFG_percent),
    Def_TOV_percent = as.numeric(Def_TOV_percent),
    Def_FT_FGA = as.numeric(Def_FT_FGA),
    win_percentage = W / (W +L)
  ) |>
  filter(Team != 'League Average')


###############################################################################

# Web Scraping Offensive Stats


basicSeasonStats <- tibble()

for (i in seq(2000, 2024)) {
  url <- paste0(
    "https://www.basketball-reference.com/leagues/NBA_",
    i,
    ".html#all_confs_standings_E"
  )
  
  web_data <-
    read_html(url) |>
    html_nodes(xpath = '//*[@id="per_game-team"]') |>
    html_table()
  
  web_df <- web_data[[1]]
  
  web_df_clean <-
    web_df |>
    janitor::clean_names(case = 'none') |>
    mutate(Season = i)
  
  basicSeasonStats <- bind_rows(basicSeasonStats, web_df_clean)
}


# Data Cleaning

basicSeasonStats <- basicSeasonStats |>
  select(-G, -MP, -Rk) |>
  mutate(
    Assist_TO = AST / TOV,
    Playoff = ifelse(grepl("\\*", Team), 1, 0)
  )


################################################################################

# Web Scraping Defensive Stats

basicSeasonStatsDef <- tibble()

for (i in seq(2000, 2024)) {
  url <- paste0(
    "https://www.basketball-reference.com/leagues/NBA_",
    i,
    ".html#all_confs_standings_E"
  )
  
  web_data <-
    read_html(url) |>
    html_nodes(xpath = '//*[@id="per_game-opponent"]') |>
    html_table()
  
  web_df <- web_data[[1]]
  
  web_df_clean <-
    web_df |>
    janitor::clean_names(case = 'none') |>
    mutate(Season = i)

  
  basicSeasonStatsDef <- bind_rows(basicSeasonStatsDef, web_df_clean)
}


# Cleaning the Data

basicSeasonStatsDef <- basicSeasonStatsDef |>
  select(-G, -MP, -PF, -Rk) |>
  rename(
    Opp_FG = FG,
    Opp_FGA = FGA,
    Opp_FG_percent = FG_percent,
    Opp_3P = X3P,
    Opp_3PA = X3PA,
    Opp_3P_percent = X3P_percent,
    Opp_2P = X2P,
    Opp_2PA = X2PA,
    Opp_2P_percent = X2P_percent,
    Opp_FT = FT,
    Opp_FTA = FTA,
    Opp_FT_percent = FT_percent,
    Off_Reb_Allowed = ORB,
    Opp_Total_Reb = TRB,
    Points_Allowed = PTS,
    TO_Forced = TOV,
    Opp_STL = STL,
    Opp_BLK = BLK,
    Opp_DRB = DRB,
    Opp_AST = AST
  )


###############################################################################

# Web scrap the top 10 Scorers from every season

topScorers <- tibble()

for (i in seq(2000, 2024)) {
  url <- paste0(
    "https://www.basketball-reference.com/leagues/NBA_",
    i,
    "_per_game.html#per_game_stats::pts_per_g"
  )
  
  web_data <-
    read_html(url) |>
    html_nodes(xpath = '//*[@id="per_game_stats"]') |>
    html_table()
  
  web_df <- web_data[[1]]
  
  web_df_clean <-
    web_df |>
    janitor::clean_names(case = 'none') |>
    mutate(Season = i)
  
  
  topScorers <- bind_rows(topScorers, web_df_clean)
}


topScorers <- topScorers |>
  select(Season, Rk, Player, Team, PTS) |>
  filter(Rk <= 10) |>
  mutate(                             # Change Abbreviations to full team name
    Team = case_when(
      Team == 'BOS' ~ 'Boston Celtics',
      Team == 'BRK' ~ 'Brooklyn Nets',
      Team == 'NYK' ~ 'New York Knicks',
      Team == 'PHI' ~ 'Philadelphia 76ers',
      Team == 'TOR' ~ 'Toronto Raptors',
      Team == 'CHI' ~ 'Chicago Bulls',
      Team == 'CLE' ~ 'Cleveland Cavaliers',
      Team == 'DET' ~ 'Detroit Pistons',
      Team == 'IND' ~ 'Indiana Pacers',
      Team == 'MIL' ~ 'Milwaukee Bucks',
      Team == 'ATL' ~ 'Atlanta Hawks',
      Team == 'CHO' ~ 'Charlotte Hornetts',
      Team == 'MIA' ~ 'Miami Heat',
      Team == 'ORL' ~ 'Orlando Magic',
      Team == 'WAS' ~ 'Washington Wizards',
      Team == 'DEN' ~ 'Denver Nuggets',
      Team == 'MIN' ~ 'Minnesota Timberwolves',
      Team == 'OKC' ~ 'Oklahoma City Thunder',
      Team == 'POR' ~ 'Portland Trail Blazers',
      Team == 'UTA' ~ 'Utah Jazz',
      Team == 'GSW' ~ 'Golden State Warriors',
      Team == 'LAC' ~ 'Los Angeles Clippers',
      Team == 'LAL' ~ 'Los Angeles Lakers',
      Team == 'PHO' ~ 'Phoenix Suns',
      Team == 'SAC' ~ 'Sacramento Kings',
      Team == 'DAL' ~ 'Dallas Mavericks',
      Team == 'HOU' ~ 'Houston Rockets',
      Team == 'MEM' ~ 'Memphis Grizzlies',
      Team == 'NOP' ~ 'New Orleans Pelicans',
      Team == 'SAS' ~ 'San Antonio Spurs',
      Team == 'SEA' ~ 'Seattle SuperSonics',
      Team == 'NJN' ~ 'New Jersey Nets',
      Team == 'NOH' ~ 'New Orleans Hornets', 
      Team == 'VAN' ~ 'Vancouver Grizzlies',
      Team == 'CHA' ~ 'Charlotte Bobcats'
    )
  )



# Change players that played for two teams to the team they ended the season with
topScorers <- topScorers |>
  mutate(
    Team = ifelse(Player == 'Ray Allen' & is.na(Team), 'Seattle SuperSonics', Team),
    Team = ifelse(Player == 'Vince Carter' & is.na(Team), 'New Jersey Nets', Team),
    Team = ifelse(Player == 'Allen Iverson' & is.na(Team), 'Denver Nuggets', Team),
    Team = ifelse(Player == 'Carmelo Anthony' & is.na(Team), 'New York Knicks', Team),
    Team = ifelse(Player == 'DeMarcus Cousins' & is.na(Team), 'New Orleans Pelicans', Team),
    Team = ifelse(Player == 'Kevin Durant' & is.na(Team), 'Phoenix Suns', Team)
    )

# Deleting duplicates
topScorers <- topScorers[-c(41,42, 62, 63, 83, 84, 120, 121, 185, 186, 249, 250), ]

# Save to a csv
write.csv(topScorers, "Top_10_Scorers.csv")

###############################################################################

# Web Scrap the top 10 rebounders from the past seasons

topRebounders <- tibble()

for (i in seq(2000, 2024)) {
  url <- paste0(
    "https://www.basketball-reference.com/leagues/NBA_",
    i,
    "_per_game.html#per_game_stats::trb_per_g"
  )
  
  web_data <-
    read_html(url) |>
    html_nodes(xpath = '//*[@id="per_game_stats"]') |>
    html_table()
  
  web_df <- web_data[[1]]
  
  web_df_clean <-
    web_df |>
    janitor::clean_names(case = 'none') |>
    mutate(Season = i) |>
    arrange(desc(TRB))
  
  
  topRebounders <- bind_rows(topRebounders, web_df_clean)
}

topRebounders <- topRebounders |>
  select(Season, Player, Team, TRB) |>
  group_by(Season) |>
  slice_head(n = 10) |>
  ungroup()


topRebounders <- topRebounders |>
  mutate(                             # Change Abbreviations to full team name
    Team = case_when(
      Team == 'BOS' ~ 'Boston Celtics',
      Team == 'BRK' ~ 'Brooklyn Nets',
      Team == 'NYK' ~ 'New York Knicks',
      Team == 'PHI' ~ 'Philadelphia 76ers',
      Team == 'TOR' ~ 'Toronto Raptors',
      Team == 'CHI' ~ 'Chicago Bulls',
      Team == 'CLE' ~ 'Cleveland Cavaliers',
      Team == 'DET' ~ 'Detroit Pistons',
      Team == 'IND' ~ 'Indiana Pacers',
      Team == 'MIL' ~ 'Milwaukee Bucks',
      Team == 'ATL' ~ 'Atlanta Hawks',
      Team == 'CHO' ~ 'Charlotte Hornetts',
      Team == 'MIA' ~ 'Miami Heat',
      Team == 'ORL' ~ 'Orlando Magic',
      Team == 'WAS' ~ 'Washington Wizards',
      Team == 'DEN' ~ 'Denver Nuggets',
      Team == 'MIN' ~ 'Minnesota Timberwolves',
      Team == 'OKC' ~ 'Oklahoma City Thunder',
      Team == 'POR' ~ 'Portland Trail Blazers',
      Team == 'UTA' ~ 'Utah Jazz',
      Team == 'GSW' ~ 'Golden State Warriors',
      Team == 'LAC' ~ 'Los Angeles Clippers',
      Team == 'LAL' ~ 'Los Angeles Lakers',
      Team == 'PHO' ~ 'Phoenix Suns',
      Team == 'SAC' ~ 'Sacramento Kings',
      Team == 'DAL' ~ 'Dallas Mavericks',
      Team == 'HOU' ~ 'Houston Rockets',
      Team == 'MEM' ~ 'Memphis Grizzlies',
      Team == 'NOP' ~ 'New Orleans Pelicans',
      Team == 'SAS' ~ 'San Antonio Spurs',
      Team == 'SEA' ~ 'Seattle SuperSonics',
      Team == 'NJN' ~ 'New Jersey Nets',
      Team == 'NOH' ~ 'New Orleans Hornets',
      Team == 'VAN' ~ 'Vancouver Grizzlies',
      Team == 'CHA' ~ 'Charlotte Bobcats'
    )
  )


# Change players that played for two teams to the team they ended the season with
topRebounders <- topRebounders |>
  mutate(
    Team = ifelse(Player == 'Dikembe Mutombo' & is.na(Team), 'Philadelphia 76ers', Team),
    Team = ifelse(Player == 'Tyson Chandler' & is.na(Team), 'New Orleans Hornets', Team),
    Team = ifelse(Player == 'Marcus Camby' & is.na(Team), 'Portland Trail Blazers', Team),
    Team = ifelse(Player == 'DeAndre Jordan' & is.na(Team), 'New York Knicks', Team),
    Team = ifelse(Player == 'Andre Drummond' & is.na(Team) & Season == 2020, 'Cleveland Cavaliers', Team),
    Team = ifelse(Player == 'Andre Drummond' & is.na(Team) & Season == 2021, 'Los Angeles Lakers', Team),
    Team = ifelse(Player == 'Domantas Sabonis' & is.na(Team), 'Sacramento Kings', Team),
    Team = ifelse(Player == 'Nikola Vučević' & is.na(Team), 'Chicago Bulls', Team)
  )

# Save to a csv
write.csv(topRebounders, "Top_10_Rebounders.csv")

###############################################################################

# Import Custom Playoff CSV

playoff_data <- read_csv('NBA_Playoff_Data.csv')



###############################################################################

# Combining the tables

BasicRegularSeasonStats <- inner_join(basicSeasonStats, basicSeasonStatsDef,
                                      by = c('Season', 'Team'))

RegularSeasonStats <- inner_join(BasicRegularSeasonStats, advancedStats,
                                 by = c('Season', 'Team')) |>
                      mutate(
                          Team = str_replace_all(Team, '\\*', '')
                      )

RegularSeasonStats <- left_join(RegularSeasonStats, playoff_data,
                                by = c('Season', 'Team'))


###############################################################################

# Create field for top scorers

RegularSeasonStats <- RegularSeasonStats |>
  mutate(Top10_Scorer = 0,
         Top10_Rebounder = 0)


# Populate top 10 scorer field
for(i in 1:nrow(topScorers)){
  
  # Grab Season and Team values
  season_val <- topScorers$Season[i]
  team_val <- topScorers$Team[i]
  
  # Find the index in the main df that has the same team and season
  match_index <- which(RegularSeasonStats$Season == season_val & RegularSeasonStats$Team == team_val)
  
  # Increment field by 1
  if (length(match_index) > 0) {
    RegularSeasonStats$Top10_Scorer[match_index] <- RegularSeasonStats$Top10_Scorer[match_index] + 1
  }
}


# Populate new top 10 rebounder field
for(i in 1:nrow(topRebounders)){
  
  # Grab Season and Team
  season_val <- topRebounders$Season[i]
  team_val <- topRebounders$Team[i]
  
  # Find Index in main df that has matching team and season values
  match_index <- which(RegularSeasonStats$Season == season_val & RegularSeasonStats$Team == team_val)
  
  # Increment the field by 1
  if (length(match_index) > 0) {
    RegularSeasonStats$Top10_Rebounder[match_index] <- RegularSeasonStats$Top10_Rebounder[match_index] + 1
  }
}


###############################################################################

# Save Final Regular Season Stats Data Frame as a csv
write.csv(RegularSeasonStats, "Regular_Season_Stats.csv")



