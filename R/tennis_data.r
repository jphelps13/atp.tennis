
##
storeFilesDt <- function(data_path, pattern, ignore = c()){
  files   <- list.files(path    = data_path,
                        pattern = pattern)
  files_dt <- data.table(file = files)
  set(files_dt, 
      j = "file", 
      value = file.path(data_path, files_dt[, file]))
  if(length(ignore) > 0){
    for(i in ignore){
      files_dt <- subset(files_dt, !grepl(i, file))  
    }
  }
  return(files_dt)
}

##
preFilterPlayers <- function(pre_filter_players, output,
                             player_file_name, ranking_files_dt, max_rank,
                             min_rank_period){
  
  if(pre_filter_players){
    ## player roster:
    player_dt <- loadPlayerDt(file_name = player_file_name)
    
    ## ranking files:
    if(pre_filter_players){
      filter_rank <- TRUE
      filter_players <- FALSE
    }
    ranking_dt <- loadRankingsDt(ranking_files_dt = ranking_files_dt,
                                 max_rank = max_rank, player_dt,
                                 filter_rank, filter_players) 
    ranking_dt[, date := ymd(date)]
    
    # average time stamp
    ranking_dt <- ranking_dt[order(+date)]
    pl_dt <- ranking_dt[, .(d = quantile(diff(as.numeric(date)),0.05),
                   n = .N), by = player_id]
    pl_dt <- pl_dt[order(+n)]
    #pl_dt$simple_name <- player_dt$simple_name[match(pl_dt$player_id, player_dt$player_id)]
    #View(pl_dt)
    pl_dt[, est_period := d * n]
    pl_dt <- pl_dt[n >= 10 & est_period >= min_rank_period,]
   
    ## subset and save
    player_dt <- subset(player_dt, player_id %in% pl_dt[, player_id])
    
    ## classes
    fac_cols <- c("player_id", "hand", "country", "name", "simple_name", 
                  "initials")
    for(j in fac_cols){
      set(player_dt, j = j, 
          value = as.factor(player_dt[, j, with = FALSE][[1]]))
    }
    
    setkey(player_dt, "player_id")
    
    saveRDS(player_dt, file = output)
  }
  
  return(invisible())
}

##
createRankingDt <- function(player_dt, ranking_files_dt){
  
  ## ranking data
  filter_rank    <- FALSE
  filter_players <- TRUE
  ranking_dt <- loadRankingsDt(ranking_files_dt = ranking_files_dt,
                               max_rank = max_rank, player_dt,
                               filter_rank, filter_players) 
  ## join
  dt <- player_dt[ranking_dt]
  
  ## clean ranking data
  cleanRankingDt <- function(dt){
    # dates
    setkey(dt, date)
    set(dt, j = "date", value = ymd(dt[, date]))
    setkey(dt, birth_date)
    set(dt, j = "birth_date", value = ymd(dt[, birth_date]))
    # factors
    setkey(dt, player_id)
    return(dt)
  }
  dt <- cleanRankingDt(dt)
 
  ## add points gain/loss
  rankingDiffDt <- function(dt){
    setkey(dt, player_id)
    dt <- dt[order(date),]
    dt[, point_change := c(0, diff(points)), by = player_id]
  }
  dt <- rankingDiffDt(dt)
  
  return(dt) 
}

##
createMatchDt <- function(player_rds, match_files_dt){
  
  ## player data
  player_dt <- readRDS(player_rds)
  
  ## ranking data
  match_dt <- loadMatchDt(match_files_dt, player_dt) 
  
  ## join
  dt <- player_dt[ranking_dt]
  
  ## clean ranking data
  cleanRankingDt <- function(dt){
    # dates
    setkey(dt, date)
    set(dt, j = "date", value = ymd(dt[, date]))
    setkey(dt, birth_date)
    set(dt, j = "birth_date", value = ymd(dt[, birth_date]))
    # factors
    setkey(dt, player_id)
    fac_cols <- c("player_id", "hand", "country", "name", "simple_name", 
                  "initials")
    for(j in fac_cols){
      set(dt, j = j, value = as.factor(dt[, j, with = FALSE][[1]]))
    }
    return(dt)
  }
  
  dt <- cleanRankingDt(dt)
  
  return(dt) 
}


##
loadPlayerDt <- function(file_name = ""){
  
  ## load 
  dt <- fread(file_name)
  
  ## set column names
  playerColNames <- function(dt){
    dt_names <- c("player_id", "forename", "surname", 
                  "hand", "birth_date", "country")
    setnames(dt , dt_names)
    return(dt)
  }
  dt <- playerColNames(dt)
  
  ## make new name
  set(dt, 
      j = "name", 
      value = sprintf("%s %s", dt[, forename], dt[,surname]))
  set(dt, 
      j = "simple_name", 
      value = sprintf("%s_%s", 
                      tolower(dt[, forename]), tolower(dt[,surname])))
  set(dt, 
      j = "initials", 
      value = sprintf("%s%s", 
                      toupper(substr(dt[, forename], 1, 1)), 
                      toupper(substr(dt[, surname], 1, 1))))
  
  ## drop columns
  dt <- dt[, ":="(forename = NULL, surname = NULL)]
  
  ## set key
  setkey(dt, "player_id")
  
  return(dt)
}


## 
loadRankingsDt <- function(ranking_files_dt, max_rank = 20, player_dt,
                           filter_rank, filter_players){
  
  ## internal cleaning functions
  rankingColNames <- function(dt){
    dt_names <- c("date", "ranking", "player_id", "points")
    setnames(dt , dt_names)
    return(dt)
  }
  readCleanDt <- function(file, max_rank, player_dt, filter_rank, filter_players){
    # load
    dt <- fread(file[[1]])
    # naming
    dt <- rankingColNames(dt)
    # filter
    if(filter_rank){
      setkey(dt, ranking)
      dt <- dt[ranking <= max_rank]
    }
    if(filter_players){
      setkey(dt, player_id)
      dt <- subset(dt, player_id %in% player_dt[, player_id])
    }
    # set points to integer
    set(dt, j = "points", value = as.integer(dt[, points]))
    # date - to do
    #cat(str(dt))
    return(dt)
  }
  
  ## run
  dt <- ranking_files_dt[, readCleanDt(file, max_rank, player_dt, 
                                       filter_rank, filter_players), 
                         by = file]
  dt <- dt[, file := NULL]
  
  ## plyer_id to factor
  j <- "player_id"
  set(dt, j = j,
      value = as.factor(dt[, j, with = FALSE][[1]]))
  
  ## set key
  setkey(dt, "player_id")
  
  return(dt)
}

##
scrapeMatchDataTournament <- function(match_files_dt, tournament_info_name){
  
  ## internal cleaning functions
  keepMatchCols <- function(){
    dt_names <- c("tourney_id", "tourney_name", "surface", "draw_size",
                  "tourney_level", "tourney_date")
    return(dt_names)
  }
  readDt <- function(file){
    keep_columns <- keepMatchCols()
    # load
    dt <- unique(fread(file[[1]],
                       select = keep_columns))
    # filter
    setkey(dt, tourney_level)
    # no davis cup; no year end; no olympics
    dt <- subset(dt, tourney_level != "D")
    #dt <- subset(dt, draw_size != 8)
    # date - to do
    #cat(str(dt))
    return(dt)
  }
  
  ## run
  dt <- match_files_dt[, readDt(file), 
                       by = file][,file := NULL]
  
  ## clean
  cleanDt <- function(dt){
    ## make new unique identifier for tournament
    setkey(dt, tourney_id)
    set(dt, 
        j = "year",
        value = str_split(dt[, tourney_id], "-", simplify = TRUE)[,1])
    
    ## remove special characters, spaces, tolower etc
    set(dt, j = "simple_tourney_name",
        value = tolower(dt[, tourney_name]))
    set(dt, j = "simple_tourney_name",
        value = gsub("'", "", dt[, simple_tourney_name]))
    set(dt, j = "simple_tourney_name",
        value = gsub("\\s+", "_", dt[, simple_tourney_name]))
    
    ## date, and month
    setkey(dt, tourney_date)
    dt <- dt[!is.na(tourney_date)]
    set(dt, j = "tourney_date", value = ymd(dt[, tourney_date]))
    set(dt, 
        j = "month",
        value = str_split(dt[, tourney_date], "-", simplify = TRUE)[,2])
    set(dt, 
        j = "month_name",
        value = month.abb[as.integer(dt[, month])])
    
    ## check if tournament year matches to observed year
    set(dt, 
        j = "year_check",
        value = str_split(dt[, tourney_date], "-", simplify = TRUE)[,1])
    dt[year_check != year & year > year_check, 
       tourney_date := ymd(sprintf("%s-%s-%s", year, 01, 01))]
    dt[year_check != year & year < year_check, 
       tourney_date := ymd(sprintf("%s-%s-%s", year, 12, 31))]
    dt[, year_check := NULL]
    
    ## check each tournament has no duplicates
    if(F){
      check <- dt[, .N, by = c("tourney_name", "surface", "year", "month")]
    }
    
    ## create id
    id_cols <- c("simple_tourney_name", "surface")
    lookup <- unique(dt[, id_cols, with = FALSE])
    set(lookup, j = "unique_tourney_id", value = as.factor(lookup[, (1:.N)]))
    
    ## merge
    setkey(lookup, simple_tourney_name, surface)
    setkey(dt, simple_tourney_name, surface)
    dt <- lookup[dt]
    
    ## filter
    dt <- subset(dt, !grepl("olympics", simple_tourney_name))
    
    ## overwrite surface
    tournament_input <- fread(tournament_info_name)
    dt <- merge(dt, tournament_input, by = "simple_tourney_name",
                all.x = TRUE)
    dt[is.na(surface.y), surface := surface.x]
    dt[!is.na(surface.y), surface := surface.y][,":="(surface.x = NULL,
                                                      surface.y = NULL)]
    
    ## classes
    fac_cols <- c("simple_tourney_name", "surface",
                  "tourney_id", "tourney_name", "tourney_level", "month_name")
    for(j in fac_cols){
      set(dt, j = j, 
          value = as.factor(dt[, j, with = FALSE][[1]]))
    }
    set(dt, j = "month", value = as.integer(dt[, month]))
    return(dt)
  }
  dt <- cleanDt(dt)
  
  ## set key
  setkey(dt, "tourney_id")
  
  # return
  return(dt)
}

##
scrapeMatchDataPerson <- function(player_dt, match_files_dt){
  
  ## internal cleaning functions
  keepMatchCols <- function(){
    dt_names <- c("winner_id", "winner_ht")
    return(dt_names)
  }
  readDt <- function(file, player_dt){
    keep_columns <- keepMatchCols()
    # load
    dt <- unique(fread(file[[1]],
                       select = keep_columns))
    # filter
    setkey(dt, winner_id)
    dt <- subset(dt, winner_id %in% player_dt[, player_id])
    dt <- subset(dt, !is.na(winner_ht))
    # date - to do
    #cat(str(dt))
    return(dt)
  }
  
  ## run
  dt <- unique(match_files_dt[, readDt(file, player_dt), 
                       by = file][,file := NULL])

  ## set key
  setnames(dt, c("winner_id", "winner_ht"), c("player_id", "height"))
  j <- "player_id"
  set(dt, j = j, 
      value = as.factor(dt[, j, with = FALSE][[1]]))
  setkey(dt, "player_id")
  
  player_dt <- dt[player_dt]
  
  # return
  return(player_dt)
}

##
scrapeMatchDataMatches <- function(player_dt, match_files_dt){
  
  ## internal cleaning functions
  keepMatchCols <- function(){
    dt_names <- c("tourney_id", "winner_id", "winner_rank", "winner_seed",
                  "winner_age", "winner_ioc", "loser_id", "loser_rank", 
                  "loser_seed", "loser_age", "loser_ioc", "score",
                  "best_of", "round", "minutes", "w_ace",
                  "w_df", "w_svpt", "w_1stIn", "w_1stWon", 
                  "w_2ndWon", "w_bpSaved", "w_bpFaced", "l_ace",
                  "l_df", "l_svpt", "l_1stIn", "l_1stWon", 
                  "l_2ndWon", "l_bpSaved", "l_bpFaced")
    dt_classes <- c("as.character", "as.character", "as.integer", "as.integer", 
                    "as.numeric", "as.character", "as.character", "as.integer", 
                    "as.integer", "as.numeric", "as.character", "as.character",
                    "as.integer", "as.character", "as.integer", "as.integer",
                    "as.integer", "as.integer", "as.integer", "as.integer",
                    "as.integer", "as.integer", "as.integer", "as.integer",
                    "as.integer", "as.integer", "as.integer", "as.integer",
                    "as.integer", "as.integer", "as.integer")
    dt <- data.table(column = dt_names, class = dt_classes)
    return(dt)
  }
  readDt <- function(file, player_dt){
    dt_info <- keepMatchCols()
    # load
    dt <- unique(fread(file[[1]],
                       select = dt_info[, column]))
    # filter
    setkey(dt, winner_id, loser_id)
    dt <- subset(dt, winner_id %in% player_dt[, player_id] |
                      loser_id %in% player_dt[, player_id])
    # classes
    for(j in names(dt)){
      change <- dt_info[column == j, class]
      set(dt, j = j, 
          value = do.call(get(change), list(dt[, j, with = FALSE][[1]])))
    }
    
    # date - to do
    # cat(str(dt))
    return(dt)
  }
  
  ## run
  dt <- unique(match_files_dt[, readDt(file, player_dt), 
                              by = file][,file := NULL])
  
  ## clean
  cleanDt <- function(dt){
    ## classes
    fac_cols <- c("tourney_id", "winner_id", "winner_ioc", "loser_id", 
                  "loser_ioc", "round")
    for(j in fac_cols){
      set(dt, j = j, 
          value = as.factor(dt[, j, with = FALSE][[1]]))
    }
    
    return(dt)
  }
  dt <- cleanDt(dt)
  
  ## set key
  setkey(dt, "tourney_id")
  
  # return
  return(dt)
}

##
transformMatchDt <- function(match_dt){
  
  ## make a "match_id"
  set(match_dt, j = "match_id", value = match_dt[, (1:.N)])
  
  ## reshape the data
  dt_win_cols <- c("tourney_id", names(match_dt)[grepl("winner", names(match_dt))],
                   "loser_id", "loser_rank", "loser_seed", "loser_age", "loser_ioc",
                   "score", "best_of", "round", "match_id", "minutes",
                   names(match_dt)[grepl("w_", names(match_dt))],
                   "l_bpSaved", "l_bpFaced")
  dt_lose_cols <- c("tourney_id", names(match_dt)[grepl("loser", names(match_dt))],
                    "winner_id", "winner_rank", "winner_seed", "winner_age", "winner_ioc",
                   "score", "best_of", "round", "match_id", "minutes",
                   names(match_dt)[grepl("l_", names(match_dt))],
                   "w_bpSaved", "w_bpFaced")
  dt_win  <- match_dt[, dt_win_cols, with = FALSE][, status := "win"]
  dt_lose <- match_dt[, dt_lose_cols, with = FALSE][, status := "lose"]
  
  ## rename
  rename_cols <- c("tourney_id", "player_id", "rank", "seed", "age", "country",
                   "opp_player_id", "opp_rank", "opp_seed", "opp_age", "opp_country",
                   "score", "best_of", "round", "match_id", "minutes",
                   "ace", "df", "svpt", "firstIn", "firstWon", "secondWon", 
                   "bpSaved", "bpFaced", "bpWon", "bpMade")
  setnames(dt_win, dt_win_cols, rename_cols)
  setnames(dt_lose, dt_lose_cols, rename_cols)
  
  ## bind, lose, gc
  dt <- rbindlist(list(dt_win, dt_lose))
  ## turn saved to won
  dt[, bpWon := bpMade - bpWon]
  ## find games
  v <- str_extract_all(dt[, score], "([0-9]+(?=-))")
  v <- vapply(v, FUN = function(x){
    sum(as.numeric(x))
  }, FUN.VALUE = numeric(1))
  set(dt, j = "games_won", value = v)

  v <- str_extract_all(dt[, score], "((?<=-)[0-9]+)")
  v <- vapply(v, FUN = function(x){
    sum(as.numeric(x))
  }, FUN.VALUE = numeric(1))
  set(dt, j = "games_lost", value = v)
  
  rm(match_dt, dt_win, dt_lose); gc()
  setkey(dt, "tourney_id")
  
  return(dt)
  
}

##
updateMatchDt <- function(player_dt, tournament_dt, match_dt){
  
  ## join to tournament info
  match_dt <- tournament_dt[match_dt, nomatch=0]
  setkey(match_dt, "player_id")
  
  ## join to sample_dt
  match_dt <- player_dt[, c("player_id", "simple_name")][match_dt, nomatch = 0]
  match_dt <- match_dt[order(tourney_date)]
  
  return(match_dt)
  
}

##
samplePlayerMatchDt <- function(player_dt, tournament_dt, match_dt,
                    player = c("andy_murray")){
  
  ## filter player_dt
  sample_dt <- subset(player_dt, simple_name %in% player)
  
  ## filter match_dt
  match_dt_f <- subset(match_dt, player_id %in% sample_dt[, player_id])
  
  ## join to tournament info
  match_dt_f <- tournament_dt[match_dt_f, nomatch=0]
  setkey(match_dt_f, "player_id")
  
  ## join to sample_dt
  sample_dt <- match_dt_f[sample_dt[, c("player_id", "simple_name")]]
  sample_dt <- sample_dt[order(tourney_date)]
  
  return(sample_dt)
  
}
  
##
playerMatchStats <- function(match_dt, min_stats = 0.5){
  
  
  ff <- match_dt[, length(unique(rank)), by = c("unique_tourney_id", 
                                                "tourney_date",
                                                "player_id")]
  ff <- match_dt[, .N, by = round]
  
  ## key
  setkey(match_dt, unique_tourney_id, player_id, tourney_date, month, year)
  
  ## filter
  filter_dt <- match_dt[, .(propNa = sum(!is.na(ace))/ length(ace)), 
                        keyby = player_id]
  filter_dt <- filter_dt[propNa >= min_stats]
  
  ## by tournament
  info_dt <- match_dt[, 
                      .( simple_name = simple_name[1],
                           tourney_name = tourney_name[1],
                           simple_tourney_name = simple_tourney_name[1],
                           age = age[1],
                           matches = .N,
                           serves = sum(svpt),
                           win_perc = sum(status == "win")/.N,
                           ace_prob = sum(ace, na.rm = T)/
                             sum(svpt, na.rm = TRUE),
                           ranking = rank[1],
                           seed = seed[1],
                           winner = any(round[status == "win"] == "F"),
                           firstIn = sum(firstIn, na.rm = T)/
                             sum(svpt, na.rm = TRUE),
                           firstWon = sum(firstWon, na.rm = T)/
                             sum(firstIn, na.rm = TRUE),      
                           secondWon = sum(secondWon, na.rm = T)/
                             sum((svpt - firstIn), na.rm = TRUE), 
                           bpFaced = sum(bpFaced),
                           bpSavePerc = sum(bpSaved)/sum(bpFaced),
                           bpMade = sum(bpMade),
                           bpWonPerc = sum(bpWon)/sum(bpMade),
                           games_won = sum(games_won),
                           total_games = sum(games_won + games_lost)
  ),
  keyby = c("unique_tourney_id", "player_id",
         "tourney_date", "month", "year")]
  
  info_dt[, games_won_perc := games_won/total_games]
  info_dt[, winner := ifelse(winner == FALSE, 0, 1)]
  
  return(info_dt)
  
}


## get data for player t-sne
playerTsneData <- function(info_dt, tournament_dt,  year_filter = NULL, 
                           max_rank = 100, min_match = 20){
  
  # # year filter
  # if(is.null(year_filter)){
  #   year_filter <- c("2011","2012","2013" ,"2014", "2015", "2016")
  # }
  # input_dt <- subset(info_dt, year %in% year_filter)
  # 
  input_dt <- info_dt

  
  # add surface
  input_dt <- merge(input_dt, unique(tournament_dt[, .(unique_tourney_id, surface)]),
                   by = "unique_tourney_id", all.x = TRUE)
  
  
  input_dt <- input_dt[, year := as.numeric(year)]
  input_dt <- input_dt[year >= 1991,]
  
  ## by tournament
  tsne_dt <- input_dt[, .( simple_name = simple_name[1],
                           matches = sumn(matches),
                           serves = sumn(serves),
                           win_perc = sumn(win_perc * matches)/sumn(matches),
                           ace_prob = sumn(ace_prob * serves)/sumn(serves),
                           ranking = meann(ranking),
                           winner = sumn(winner),
                           firstIn = sumn(firstIn * serves)/sumn(serves),
                           firstWon = sumn(firstWon * serves)/sumn(serves),      
                           secondWon = sumn(secondWon * serves)/sumn(serves), 
                           bpFaced = sumn(bpFaced),
                           bpSavePerc = sumn(bpSavePerc * bpFaced)/sumn(bpFaced),
                           bpMade = sumn(bpMade),
                           bpWonPerc = sumn(bpWonPerc * bpMade)/sumn(bpMade),
                           games_won = sumn(games_won),
                           total_games = sumn(total_games)
  ),
  keyby = c("player_id", "surface")][order(+ranking)]
  
  tsne_dt <- tsne_dt[ranking <= max_rank,]
  tsne_dt <- tsne_dt[surface != "" & surface!= "Carpet"]
  
  # number of matches per surface
  tsne_dt <- tsne_dt[, nrows := .N, by = player_id]
  tsne_dt <- tsne_dt[nrows == max(nrows),][,nrows := NULL]
  tsne_dt <- tsne_dt[, min_matches := min(matches), by = player_id]
  tsne_dt <- tsne_dt[min_matches >= min_match,][,min_matches := NULL]
  
  # reshape
  id_cols <- c("player_id", "surface", "simple_name")
  relative_cols <- c("win_perc")
  #, "ace_prob", "firstIn", "bpSavePerc", "bpWonPerc"
  absolute_cols <- c()
  
  # turn in to relative performance
  tsne_dt <- tsne_dt[, (relative_cols) := lapply(.SD, FUN = function(x){
    x/(sum(x*matches)/sum(matches))
  }), by = .(player_id), .SDcols = relative_cols]
  
  # # get averages for these columns
  # tsne_dt <- tsne_dt[, (absolute_cols) := lapply(.SD, FUN = function(x){
  #   sum(x*matches)/sum(matches)
  # }), by = .(player_id), .SDcols = absolute_cols]
  
  # filter
  tsne_dt <- tsne_dt[, c(id_cols, relative_cols, absolute_cols), with = FALSE]
  
  
  # reshape
  f <- "player_id + simple_name"
  # for(i in absolute_cols){
  #   f <- sprintf("%s + %s", f, i)
  # }
  f <- formula(sprintf("%s ~ %s", f, "surface"))
  tsne_dt <- dcast(tsne_dt, f, 
        value.var = relative_cols)
  
  # add ranking
  rank_dt <- input_dt[, .(ranking = median(ranking, na.rm = T)), by = "player_id"]
  rank_dt <- rank_dt[!is.na(ranking),]
  tsne_dt <- merge(tsne_dt, rank_dt, by = "player_id")
  tsne_dt <- tsne_dt[order(+ranking)]
  tsne_dt <- tsne_dt[!is.na(ranking)]
  
  tsne_mat <- as.matrix(tsne_dt[, 3:(ncol(tsne_dt)-1), with = FALSE])
  tsne_lab <- tsne_dt[, c(1:2, ncol(tsne_dt)), with = FALSE]
  
  out <- list(tsne_mat = tsne_mat, tsne_lab = tsne_lab)
  return(out)
}

##
updateFunnelDt <- function(input_dt = copy(info_dt), player_dt){
  
  # add surface
  input_dt <- merge(input_dt, 
                    unique(tournament_dt[, .(unique_tourney_id, surface,
                                             tourney_level)]),
                    by = "unique_tourney_id", all.x = TRUE)
  
  
  input_dt <- input_dt[, year := as.numeric(year)]
  
  # weighting tournaments
  input_dt[, tourney_level := as.character(tourney_level)]
  weight_dt <- data.table(tourney_level = c("G", "A", "M", "F", "C"),
                          weight = c(4, 1, 2, 2.5, 1))
  setkeyv(weight_dt, "tourney_level")
  setkeyv(input_dt, "tourney_level")
  input_dt <- weight_dt[input_dt]
  input_dt[is.na(weight), weight := 1]
  
  # slams
  input_dt[, num_slams := sum(winner[tourney_level == "G"]), by = player_id]
  
  ## by tournament
  input_dt <- input_dt[order(tourney_date),]
  funnel_dt <- input_dt[, .( simple_name = simple_name,
                             matches = cumsum(matches),
                             win_perc = cumsum(win_perc * matches)/cumsum(matches),
                             win_perc_w = cumsum(win_perc * matches * weight)/
                               cumsum(matches * weight),
                             age = age,
                             tourney_date = tourney_date,
                             num_slams = num_slams
  ),
  keyby = c("player_id", "surface")]
  
  
  ## by tournament
  input_dt <- input_dt[order(tourney_date),]
  funnel_dt_all <- input_dt[, .( simple_name = simple_name,
                             matches = cumsum(matches),
                             win_perc = cumsum(win_perc * matches)/cumsum(matches),
                             win_perc_w = cumsum(win_perc * matches * weight)/
                               cumsum(matches * weight),
                             age = age,
                             tourney_date = tourney_date,
                             num_slams = num_slams
  ),
  keyby = c("player_id")]
  funnel_dt_all <- funnel_dt_all[, surface := "All"]
  setcolorder(funnel_dt_all, names(funnel_dt))
  funnel_dt <- rbind(funnel_dt, funnel_dt_all)
  
  # player name
  setkey(funnel_dt, "simple_name")
  setkey(player_dt, "simple_name")
  funnel_dt <- player_dt[, .(simple_name, name, initials)][funnel_dt]
  
  # remove blank surface
  funnel_dt <- funnel_dt[surface != ""]
  funnel_dt[, surface := as.character(surface)]
  
  # gg <- funnel_dt[simple_name == "novak_djokovic" & surface == "Clay"]
  # hh <- funnel_dt[simple_name == "nick_kyrgios" & surface == "Clay"]
  # 
  # gg <- funnel_dt[simple_name == "bjorn_borg" & surface == "Clay"]
  # hh <- funnel_dt[simple_name == "rafael_nadal" & surface == "Clay"]
  # 
  # plot(gg$age, gg$win_perc, type = "l", ylim = range(gg$win_perc, gg$win_perc_w))
  # lines(gg$age, gg$win_perc_w, type = "l", col = "red")
  # 
  # plot(gg$age, gg$win_perc_w, type = "l", ylim = range(gg$win_perc_w, hh$win_perc_w))
  # lines(hh$age, hh$win_perc_w, type = "l", col = "red")
  # 
  # plot(gg$matches, gg$win_perc_w, type = "l", ylim = range(gg$win_perc_w, hh$win_perc_w))
  # lines(hh$matches, hh$win_perc_w, type = "l", col = "red")
  
  return(funnel_dt)
}