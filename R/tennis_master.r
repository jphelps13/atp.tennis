rm(list = ls())

# R SETUP
#------------------------------------------------------------------------------#
## main dir
# root_path <- "C:/Users/jphel_000/OneDrive/Documents/GIT/cool_people_only/tennis/"
root_path <- getwd()

## other dir
code_path <- file.path(root_path, "R")
data_path <- file.path(root_path, "data")
results_path <- file.path(root_path, "results")

## source codes
source(file.path(code_path, "tennis_data.r"))
source(file.path(code_path, "tennis_functions.r"))

## private keys file, for plotly account information
# Sys.setenv("plotly_username" = "username")
# Sys.setenv("plotly_api_key" = "api_key") 
source(file.path(root_path, "vault/keys.r"))

## packages
loadPackages()

# PROCESS SETUP
#------------------------------------------------------------------------------#

max_rank <- 10
min_rank_period <- 180
pre_filter_players <- FALSE


# DATA
#------------------------------------------------------------------------------#
## file names
player_file_name <- file.path(data_path, "atp_players.csv")
ranking_files_dt <- storeFilesDt(data_path, pattern = "\\_rankings\\_")
match_files_dt   <- storeFilesDt(data_path, pattern = "\\_matches\\_",
                                 ignore = c("qual", "futures"))
player_rds <- file.path(data_path, "filter_players.rds")
tournament_info_name <- file.path(data_path, "tournament_info.csv")

## filter players
preFilterPlayers(pre_filter_players, output = player_rds,
                 player_file_name, ranking_files_dt, max_rank,
                 min_rank_period)

## player_dt
player_dt <- readRDS(player_rds)

## main data
ranking_dt <- createRankingDt(player_dt, ranking_files_dt)

## match data for players
tournament_dt <- scrapeMatchDataTournament(match_files_dt,
                                           tournament_info_name)

## update player_dt with more info
player_dt <- scrapeMatchDataPerson(player_dt, match_files_dt)

## match data (not in depth)
match_dt <- scrapeMatchDataMatches(player_dt, match_files_dt)
match_dt <- transformMatchDt(match_dt)

## merge
match_dt <- updateMatchDt(player_dt, tournament_dt, match_dt)

## extract match information
info_dt <- playerMatchStats(match_dt)

## funnel plot data
funnel_dt <- updateFunnelDt(input_dt = copy(info_dt), player_dt)
exploreFunnelPlots(funnel_dt)

## clustering
tsne_list <- playerTsneData(info_dt, tournament_dt)


## create a sample data set 
sample_dt <- samplePlayerMatchDt(player_dt, tournament_dt, match_dt,
                                  player = c("lucas_pouille"))




# testing out plotly
#------------------------------------------------------------------------------#

test_plotly_animate_ranking(ranking_dt)