countryIndexFunction <- function(ranking_dt){
  
  # get year
  set(ranking_dt, 
      j = "year",
      value = str_split(ranking_dt[, date], "-", simplify = TRUE)[,1])
  
  ranking_dt$year <- as.numeric(ranking_dt$year)
  
  # find year ranking data
  year_dt <- ranking_dt[, .(points = mean(points)), 
                        by = .(player_id, country, year)]
  
  year_dt <- year_dt[points > 0,]
  year_dt <- year_dt[, .(points = sum(points)), by = .(country, year)]
  year_dt <- year_dt[, Index := points/max(points), by = year]
  year_dt <- year_dt[order(year, -Index)]
  
  countries <- sort(unique(year_dt$country))
  
  country_dt <- year_dt[, .(Index = mean(Index)), by = country]
  country_dt <- country_dt[order(-Index)]
  
  getLoessFit <- function(x, y, smooth_index, range = 0.25, start = 0.25){
    if(smooth_index == 0){
      s_val <- y
    }else{
      span_index <- (smooth_index/(1/range))+start
      s_val <- predict(loess(y ~ x, span = span_index))
      s_val <- s_val/max(c(s_val,1))
    }
  }
  basePlotIndex <- function(dt, country_input, col){
    sdt <- subset(dt, country == country_input)
    s_val <- getLoessFit(sdt$year, sdt$Index, smooth_index)
    plot(sdt$year, s_val, type = "l", col = col,
         ylim = c(0, 1))
  }
  linePlotIndex <- function(dt, country_input, col){
    sdt <- subset(dt, country == country_input)
    s_val <- getLoessFit(sdt$year, sdt$Index, smooth_index)
    lines(sdt$year, s_val, col = col)
  }
  
  # smooth_index set between 0.25 and 0.5 in function.
  # rescaled from 0 - 1
  smooth_index <- 
  basePlotIndex(year_dt, "USA", "blue")

  par(mfrow = c(2,1))
  
  smooth_index <- 1
  basePlotIndex(year_dt, "USA", "darkgrey")
  
  linePlotIndex(year_dt, "GBR", "red")
  linePlotIndex(year_dt, "SRB", "green")
  linePlotIndex(year_dt, "FRA", "orange")
  linePlotIndex(year_dt, "ESP", "black")
  linePlotIndex(year_dt, "SUI", "purple")
  
  sum_dt <- year_dt[, .(points = sum(points)), by = year][order(year)]  
}

analyseTsneData <- function(tsne_list, filter_cols = NULL){
  
  tsne_mat <- tsne_list$tsne_mat
  
  tsne_out <- Rtsne(tsne_mat, perplexity = 5,
                    max_iter = 1000)
  plot(tsne_out$Y) # Plot the result
  
  # kmeans
  K <- 7
  k_mat <- kmeans(tsne_out$Y, centers = K)
  
  
  plot(tsne_out$Y, col = rainbow(max(k_mat$cluster))[k_mat$cluster],
       pch = 16) # Plot the result
  
  tsne_list$tsne_lab$cluster <- k_mat$cluster
  
  
  #View(tsne_list$tsne_lab)
  lab_dt <- tsne_list$tsne_lab
  lab_list <- split(lab_dt, lab_dt$cluster)
  lapply(lab_list, head, 10)
  #View(lab_dt)
  
  # pairs of tsne_mat
  pairs(tsne_mat, col = rainbow(max(k_mat$cluster))[k_mat$cluster],
        pch = 16)
  
  
  cols <- 7
  r <- rainbow(cols)
  plot(1,1, col = r[1], xlim = c(0,cols+1), pch = 16)
  for(i in 2:cols){
    points(i,1, col = r[i], pch = 16)
  }

  ## summary stats by group
  tsne_dt <- setDT(as.data.frame(tsne_mat))
  ave_cols <- names(tsne_dt)
  tsne_dt$cluster <- k_mat$cluster
  tsne_dt <- tsne_dt[, lapply(.SD, FUN = function(x){mean(x)}), 
          by = cluster, .SDcols = ave_cols]
  tsne_dt <- tsne_dt[order(cluster)]
  
  k <- match("stefan_edberg", lab_dt$simple_name)
  tsne_mat[k,]
  
  tsne_dt <- setDT(as.data.frame(tsne_mat))
  f <- cbind(lab_dt, tsne_dt)
  
}

exploreFunnelPlots <- function(funnel_dt){
  
  zz <- funnel_dt[surface == "Clay"]
  
  zz <- zz[order(age)]
  plot(zz$age, zz$matches, col = "darkgrey")
  l <- loess(zz$matches ~ zz$age, span = 0.2)
  lines(l$x, predict(l))
  v <- predict(l, newdata = 23)
  
  
  plot(zz$matches, zz$win_perc_w, col = "darkgrey")
  zz <- zz[order(matches)]
  l <- loess(zz$win_perc_w ~ zz$matches, span = 0.6)
  lines(l$x, predict(l))
  abline(v = v)
  
  sum(zz$win_perc_w*zz$matches)/sum(zz$matches)
  
  zz <- zz[order(age)]
  plot(zz$age, zz$win_perc_w, col = "darkgrey")
  l <- loess(zz$win_perc_w ~ zz$age, span = 0.2)
  lines(l$x, predict(l))
  abline(v = 23)
  
  
  abline(v = 23)
  
  plot(l$x, abs(l$y - predict(l)), col = "darkgrey") 
  
  new_dt <- CJ(matches = seq(1, max(zz$matches), 1))
  fit <- predict(l, ne)
  
  boot_lowess <- function(dt, i){
    l <- loess(dt$win_perc_w ~ dt$matches, span = 0.2)
    out <- predict(l, newdata = new_dt$matches)
    return(out)
  }
  
  #----------------------------------------------------------------------------#
  
  foo_surface <- function(players, i){
    pl_fil <- unique(players[i])
    out_dt <- funnel_dt[simple_name %in% pl_fil, 
                        .(win_perc = sumn(win_perc * matches)/sumn(matches),
                            matches = sumn(matches)),
                       by = c("surface")]
    # out_dt <- out_dt[, (relative_cols) := lapply(.SD, FUN = function(x){
    #   x/(sum(x*matches)/sum(matches))
    # }), .SDcols = relative_cols]
    out_dt <- out_dt[order(+surface),]
    
    out <- unlist(out_dt[, 2])
    return(out)
  }
  
  players <- unique(funnel_dt$simple_name)
  setkey(funnel_dt, "simple_name")
  ##
  tt2 <- boot(players, 
              statistic = foo_surface,
              R = 2000,
              stype = "i",
              ncpus = 4)
  
  # hist(tt2$t[, 4])

  
  pl0 <- 0.05
  pu0 <- 0.95
  pl <- 0.005
  pu <- 0.995
  
  
  surf_dt <- data.table(surface = sort(unique(funnel_dt$surface)))
  surf_dt[, index := 1:.N]
  surf_dt[, min := quantile(tt2$t[, index], pl), by = index]
  surf_dt[, max := quantile(tt2$t[, index], pu), by = index]
  surf_dt[, low := quantile(tt2$t[, index], pl0), by = index]
  surf_dt[, hi := quantile(tt2$t[, index], pu0), by = index]
  surf_dt[, med := quantile(tt2$t[, index], 0.5), by = index]
  setkey(surf_dt, surface)
  
  # median date
  funnel_dt[, median_date := median(tourney_date), by = player_id]
  
  # add agg
  out_dt <- copy(funnel_dt)
  out_dt <- merge(out_dt, surf_dt, by = "surface")
  
  periods <- 5
  pl_dt <- out_dt[, .(median_date = median_date[1]), by = simple_name]
  pl_dt <- pl_dt[order(median_date),]
  pl_dt[, date_group := cut(median_date, breaks = periods)]
  out_dt <- merge(out_dt, pl_dt[,.(simple_name, date_group)], 
                  by = "simple_name")
  
  #
  out_dt[, lower_95 := (low + qnorm(pl0)*sqrt(low*(1-low)/matches))]
  out_dt[, upper_95 := (hi + qnorm(pu0)*sqrt(hi*(1-hi)/matches))]
  out_dt[lower_95 < 0, lower_95 := 0]
  out_dt[upper_95 > 1, upper_95 := 1]
  
  out_dt[, lower_99 := (min + qnorm(pl)*sqrt(min*(1-min)/matches))]
  out_dt[, upper_99 := (max + qnorm(pu)*sqrt(max*(1-max)/matches))]
  out_dt[lower_99 < 0, lower_99 := 0]
  out_dt[upper_99 > 1, upper_99 := 1]
  
  
  out_dt[, col := "#E0E0E0"]
  out_dt[win_perc < lower_95, col := "#F48E75"]
  out_dt[win_perc < lower_99, col := "#AF4848"]
  out_dt[win_perc > upper_95, col := "#7ABADD"]
  out_dt[win_perc > upper_99, col := "#2E549B"]
  
  out_dt[, group := "standard"]
  out_dt[win_perc < lower_95, group := "below average"]
  out_dt[win_perc < lower_99, group := "journeyman"]
  out_dt[win_perc > upper_95, group := "above average"]
  out_dt[win_perc > upper_99, group := "world class"]
  
  out_dt[, group := factor(group, 
                           levels = c("journeyman", "below average", "standard",
                                      "above average", "world class"))]
  
  symbols <- c("x", "cross", "square", "triangle-up", "circle")
  groups <- sort(unique(out_dt$date_group))
  for(j in seq_along(groups)){
    out_dt[date_group == groups[j], symbol := symbols[j]]
  }
  out_dt[, date_group := factor(date_group, 
                           levels = levels(factor(out_dt$date_group))[
                             order(levels(factor(out_dt$date_group)))])]
  levels(out_dt$date_group) <- substr(levels(out_dt$date_group), 1, 4)
  
  out_dt[, size := 8]

  
  players <- c("carlos_moya", "gustavo_kuerten")
  p1 <- generateSigPlot(out_dt, "Clay", "#a76b29", yreg = "win_perc_w",
                        players = players)
  p11 <- generateSigPlot(out_dt, "Clay", "#a76b29", yreg = "win_perc",
                         players = players)
  p <- subplot(p1, p11)
  
  players <- c("goran_ivanisevic", "patrick_rafter")
  p1 <- generateSigPlot(out_dt, "Grass", "#a76b29", yreg = "win_perc_w",
                        players = players)
  p11 <- generateSigPlot(out_dt, "Grass", "#a76b29", yreg = "win_perc",
                         players = players)
  p <- subplot(p1, p11)
  
  p1 <- generateSigPlot(out_dt, "Hard", "#a76b29", yreg = "win_perc_w")
  p11 <- generateSigPlot(out_dt, "Hard", "#a76b29", yreg = "win_perc")
  p <- subplot(p1, p11)
  
  p1 <- generateSigPlot(out_dt, "Indoor Hard", "#a76b29", yreg = "win_perc_w")
  p11 <- generateSigPlot(out_dt, "Indoor Hard", "#a76b29", yreg = "win_perc")
  p <- subplot(p1, p11)
  
  p1 <- generateSigPlot(out_dt, "Carpet", "#a76b29", yreg = "win_perc_w")
  p11 <- generateSigPlot(out_dt, "Carpet", "#a76b29", yreg = "win_perc")
  p <- subplot(p1, p11)
  
  players <- c("ivan_lendl", "boris_becker")
  players <- c("jimmy_connors", "john_mcenroe")
  players <- c("greg_rusedski", "tim_henman", "andy_murray")
  players <- c("andy_roddick", "pete_sampras", "andre_agassi")
  p1 <- generateSigPlot(out_dt, "All", "#a76b29", yreg = "win_perc_w",
                        players = players)
  p11 <- generateSigPlot(out_dt, "All", "#a76b29", yreg = "win_perc",
                         players = players)
  p <- subplot(p1, p11)
  
  
  p2 <- generateSigPlot(out_dt, "Grass", "007B0C")
  p3 <- generateSigPlot(out_dt, "Hard", "#003366")
  p4 <- generateSigPlot(out_dt, "Indoor Hard", "#1996b3")
  
  players = 
  p0 <- generateSigPlot(out_dt, "All", "black")
  
  p1
  p2
  p3
  p4
  

  api_create(p11, filename = "clay - v2",
             sharing = "public")
    
  p <- subplot(p1, p2, p3, p4, nrows = 2) %>%
    layout(showlegend = FALSE, showlegend2 = TRUE,
           showlegend3 = FALSE, showlegend4 = TRUE)
  
  
  subplot(p1, p2, p3, nrows = 2) %>%
    layout(title = "Walmart Store Openings by Year",
           xaxis = list(domain=list(x=c(0,0.5),y=c(0,0.5))),
           scene = list(domain=list(x=c(0.5,1),y=c(0,0.5))),
           xaxis2 = list(domain=list(x=c(0.5,1),y=c(0.5,1))),
           showlegend=FALSE,showlegend2=FALSE)
  
  
  plot(fdt$matches, fdt$win_perc, col = fdt$col, pch = 16) 
  lines(fdt$matches, fdt$lower_95, lty = 2, col = "indianred1")
  lines(fdt$matches, fdt$lower_99, lty = 2, col = "indianred4")
  lines(fdt$matches, fdt$upper_95, lty = 2, col = "cadetblue1")
  lines(fdt$matches, fdt$upper_99, lty = 2, col = "cadetblue4")
  
  m <- mtcars[which.max(mtcars$mpg), ]
  
  a <- list(
    x = m$wt,
    y = m$mpg,
    text = rownames(m),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = 20,
    ay = -40
  )

  
  
}

generateSigPlot <- function(out_dt, surface0 = "All", title_color = "black",
                            op = 0.6, op_s = 0.85, yreg = "win_perc",
                            players = NULL){
  fdt <- out_dt[surface == surface0,]
  tdt <- fdt[, .SD[matches == max(matches)], by = .(player_id)]
  
  tdt <- tdt[order(+group)]
  pal <- unique(tdt$col)
  tdt <- tdt[order(+date_group)]
  sym <- unique(tdt$symbol)
  tdt <- tdt[order(+matches)]
  
  # base
  p <- plot_ly(tdt, x = ~matches, y = tdt[[yreg]], color = ~group, colors = pal,
               type = "scatter", mode = "markers", 
               marker = list(size = tdt[["size"]], line = list(color = "#353535", width = 1),
                             opacity = 0),
               text = ~simple_name,
               symbol = ~date_group, symbols = sym,
               showlegend = FALSE)
  
  # add funnels
  p <- p  %>% add_trace(x = ~matches, y = ~lower_95, mode = "lines", text = NULL,
                        line = list(color = out_dt[group == levels(out_dt$group)[2], 
                                                   col][1],
                                    dash = "dot"),
                        inherit = FALSE, showlegend = FALSE, opacity = op,
                        hoverinfo = "skip") %>%
    add_trace(x = ~matches, y = ~lower_99, mode = "lines", text = NULL,
              line = list(color = out_dt[group == levels(out_dt$group)[1], 
                                         col][1],
                          dash = "dot"),
              inherit = FALSE,  showlegend = FALSE, opacity = op,
              hoverinfo = "skip") %>%
    add_trace(x = ~matches, y = ~upper_95, mode = "lines", text = NULL,
              line = list(color = out_dt[group == levels(out_dt$group)[4], 
                                         col][1],
                          dash = "dot"),
              inherit = FALSE,  showlegend = FALSE, opacity = op,
              hoverinfo = "skip") %>%
    add_trace(x = ~matches, y = ~upper_99, mode = "lines", text = NULL,
              line = list(color  =out_dt[group == levels(out_dt$group)[5], 
                                         col][1],
                          dash = "dot"),
              inherit = FALSE,  showlegend = FALSE, opacity = op,
              hoverinfo = "skip") 
  
  # add horizontal line
  p <- p %>% add_trace(x = ~matches, y = ~med, mode = "lines", text = NULL,
                       inherit = FALSE, showlegend = FALSE, opacity = op_s,
                       line = list(color = out_dt[group == "standard", col][1],
                                   dash = "solid"),
                       hoverinfo = "skip")
  # add player line
  if(!is.null(players)){
    for(player in players){
      pl_dt <- fdt[simple_name == player]
      pl_dt <- pl_dt[matches >= max(20, min(tdt$matches)/2),]
      pl_dt[, prev := c(0, group[1:(.N-1)])]
      w <- which(as.numeric(pl_dt$group) != pl_dt$prev)
      N <- nrow(pl_dt)
      # improvement: linear interpolation to get lines
      for(i in seq_along(w)){
        add_dt <- pl_dt[w[i]:min(w[(i+1)], N, na.rm = T)]
        p <- p %>% add_trace(data = add_dt, x = ~matches, y = add_dt[[yreg]],
                             mode = "lines", text = ~simple_name, inherit = FALSE,
                             showlegend = FALSE, opacity = 1,
                             line = list(color = add_dt$col[1],
                                         dash = "solid"))
      }
    }
  }
  
  # add real points
  p <- p %>% add_trace(data = tdt, x = ~matches, y = tdt[[yreg]], color = ~group, colors = pal,
                       type = "scatter", mode = "markers", 
                       marker = list(size = ~size, line = list(color = "#353535", width = 1),
                                     opacity = op_s),
                       text = ~simple_name, inherit = FALSE,
                       symbol = ~date_group, symbols = sym,
                       showlegend = FALSE)
  if(!is.null(players)){
    for(player in players){
      pl_dt <- tdt[simple_name == player]
      p <- p %>% add_trace(data = pl_dt, x = ~matches, y = pl_dt[[yreg]], color = ~group, colors = pal,
                           type = "scatter", mode = "markers", 
                           marker = list(size = 12, line = list(color = "#353535", width = 1),
                                         opacity = 1),
                           text = ~simple_name, inherit = FALSE,
                           symbol = ~date_group, symbols = sym,
                           showlegend = FALSE)
    }
  }
  
  # build legend
  p <- p %>% add_trace(data = tdt,x = ~matches, y = tdt[[yreg]], color = ~group, colors = pal,
                       type = "scatter", mode = "markers", 
                       marker = list(size = ~size, line = list(color = "#353535", width = 1),
                                     opacity = 1), inherit = FALSE,
                       showlegend = TRUE, visible = "legendonly", legendgroup = "1") %>%
    add_trace(data = tdt,x = ~matches, y = tdt[[yreg]],
              type = "scatter", mode = "markers", 
              marker = list(size = ~size, line = list(color = "#353535", width = 1),
                            opacity = 1),
              text = ~simple_name, inherit = FALSE,
              symbol = ~date_group, symbols = sym,
              showlegend = TRUE, visible = "legendonly", legendgroup = "2")
  
  # format layout
  p <- p %>% layout(title = surface0, titlefont = list(size = 25, color = title_color),
                    yaxis = list(zeroline = FALSE),
                    xaxis = list(zeroline = FALSE), margin = list(t = 50)
  ) 
  return(p)
}

predictBinomialWins <- function(input_dt = copy(info_dt), player_dt,
                                tournament_dt){

  # add surface
  input_dt[, c("surface", "tourney_level") := NULL]
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
  
  
  
  setorder(input_dt, +age)
  input_dt[, cum_matches := cumsum(matches), by = simple_name]  
  
  
  player <- "roger_federer"
  player <- "rafael_nadal"
  player <- "andy_roddick"
  player <- "andre_agassi"
  eg <- input_dt[simple_name == player]
 
  # penalise it when cba
#   mod1 <- glmnet(win_perc ~ log(cum_matches) + cum_matches + tourney_level, 
#                  data = eg, 
#                  family = quasibinomial(),
#                  weights = matches)
#   summary(mod1)
  
  mod1 <- glm(win_perc ~  log(cum_matches) + cum_matches, 
              data = eg, 
              family = quasibinomial(),
              weights = matches)
  summary(mod1)
  eg$fit <- predict(mod1, type = "response")
  
  plot(eg$cum_matches, eg$win_perc, cex = sqrt(eg$matches),
       col = "darkgrey")
  lines(eg$cum_matches, eg$fit)
  
  plot(log(eg$cum_matches), eg$win_perc, cex = sqrt(eg$matches),
       col = "darkgrey")
  lines(log(eg$cum_matches), eg$fit)
  
  plot(eg$age, eg$win_perc, cex = sqrt(eg$matches),
       col = "darkgrey")
  lines(eg$age, eg$fit)
  
  eg$wins <- eg$matches * eg$win_perc
  eg$fit_wins <- eg$matches * eg$fit
  
  plot(eg$cum_matches, cumsum(eg$wins)/eg$cum_matches, col = "darkgrey")
  lines(eg$cum_matches, cumsum(eg$fit_wins)/eg$cum_matches)
  
  
  input_dt[, simple_name := as.character(simple_name)]
  input_dt[, surface := as.character(surface)]
  mod1 <- glm(win_perc ~ log(cum_matches)*simple_name + cum_matches*simple_name, 
              data = input_dt, 
              family = quasibinomial(),
              weights = cum_matches)
  summary(mod1)
  
  input_dt[, fit :=  predict(mod1, type = "response")]
  input_dt[, wins := matches * win_perc]
  input_dt[, fit_wins := matches * fit]
  input_dt[, cum_wins := cumsum(wins), by = simple_name]
  input_dt[, cum_fit_wins := cumsum(fit_wins), by = simple_name]
  
  ggplot(input_dt, aes(x = cum_matches, y = cum_fit_wins/cum_matches, col = simple_name)) +
    geom_line() + theme_minimal()
  
  input_dt[cum_matches < 10 & cum_fit_wins/cum_matches > 0.9]
  

  # input_dt <- input_dt[cum_matches >= 20,]
  mod1 <- glm(win_perc ~ log(cum_matches)*simple_name + 
                cum_matches*simple_name + age*simple_name + log(age)*simple_name, 
              data = input_dt, 
              family = quasibinomial(),
              weights = matches)
  summary(mod1)
  
  input_dt[, fit :=  predict(mod1, type = "response")]
  input_dt[, wins := matches * win_perc]
  input_dt[, fit_wins := matches * fit]
  input_dt[, cum_wins := cumsum(wins), by = simple_name]
  input_dt[, cum_fit_wins := cumsum(fit_wins), by = simple_name]
  
  N <- 20
  input_dt[, roll_matches := rollapply(matches, N, sum, fill = NA),
           by = simple_name]
  input_dt[, roll_wins := rollapply(wins, N, sum, fill = NA),
           by = simple_name]
  input_dt[, roll_fit_wins := rollapply(fit_wins, N, sum, fill = NA),
           by = simple_name]

  
  players <- sort(unique(input_dt$simple_name))
  i <- players[1]
  
  par(mfrow = c(3,3))
  for(i in players){
    eg <- input_dt[simple_name == i & !is.na(roll_matches)]
    plot(eg$age, eg$roll_wins/eg$roll_matches, 
         col = rgb(0, 100, 20, 30, maxColorValue=255),
         pch = 16,
         cex = 2,
         main = i)
    lines(eg$age, eg$roll_fit_wins/eg$roll_matches)
  }
  
  
  ggplot(input_dt, aes(x = cum_matches, y = cum_fit_wins/cum_matches, col = simple_name)) +
    geom_line() + theme_minimal()
  
  
}
