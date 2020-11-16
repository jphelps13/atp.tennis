# 
# person <- "roger_federer"
# 
# ff <- ranking_dt[simple_name == person]
# 
# plot(ff[, date], ff[, ranking],type = "l", ylim = c(0,50))
# 
# 

test_plotly_animate_ranking <- function(ranking_dt, players){
  
  ## filter
  players <- c("roger_federer", "andy_roddick", "rafael_nadal",
               "stanislas_wawrinka", "dominic_thiem", "andy_murray",
               "gael_monfils")
  dt <- subset(ranking_dt, simple_name %in% players)
  dt <- subset(dt, ranking <= 20)
  dt <- subset(dt, date > "2010-01-01")
  set(dt, j = "ranking_scaled", value = dt[, ranking]^2)
  
  p1 <- ggplot(data = dt,
               aes(x = date, y = ranking_scaled, col = simple_name))
  p1 <- p1 + geom_point(size = 1.7)
  p1 <- p1 + geom_text(aes(label=initials), size = 2)
  
  
  gg <- ggplot(gapminder, aes(date, ranking, color = continent)) +
    geom_point(aes(size = pop, frame = year, ids = country)) +
    
  ggplotly(gg)
  
  
  
  
}


## muck

muck <- function(info_dt, player = "andy_murray"){
  
  setkey(info_dt, tourney_date)
  
  dt <- info_dt[simple_name == player]
  
  dt[, doy := as.integer(strftime(tourney_date, format = "%j"))]
  
  year_input <- c("2016", "2015", "2014", "2013", "2012",
                  "2011", "2010")
  dt <- dt[year %in% year_input]
  
  
  loess_smooth <- function(x, y, w, span = 0.4){
    l <- loess(y~x, span = span, weights = w)
    if(F){
      plot(x,y, pch = 16, type = "b", col = "darkgrey")
      lines(x, predict(l))
    }
    return(predict(l))
  }
  
  
  dt <- dt[, oops  := loess_smooth(doy, games_won_perc, matches, 0.4), by = "year"]
  
  dt <- dt[, wp  := loess_smooth(doy, win_perc, matches, 0.4), by = "year"]
  
  
  plot_f <- function(f, dt, column){
    col <- rainbow(length(f))
    plot(f[[1]]$doy, 
         f[[1]][, column, with = FALSE][[1]], type = "l", col = col[1], 
         ylim  = range(dt[, column, with = FALSE][[1]]),
         xlim = range(dt$doy))
    
    for(i in 1:(length(f)-1)){
      
      lines(f[[(i+1)]]$doy,
            f[[(i+1)]][, column, with = FALSE][[1]], col = col[(i+1)])
      
    }
  }
  
  f <- split(dt, dt$year)
  plot_f(f, dt, "oops")
  plot_f(f, dt, "wp")

  plot(f[[1]]$doy, 
       f[[1]]$win_perc, type = "l", col = col[1], 
       ylim  = range(dt$win_perc, na.rm = TRUE),
       xlim = range(dt$doy))
  
  for(i in 1:(length(f)-1)){
    
    lines(f[[(i+1)]]$doy,
          f[[(i+1)]]$win_perc, col = col[(i+1)])
    
  }
  
  
  
  
}




## plot

eek <- function(info_dt){
  
  info_dt <- info_dt[order(tourney_date),]
  
  ## trim to 1 year for ease
  dt <- info_dt[year == "2016"]
  
  ##
  xv <- "tourney_date"
  y1v <- "win_perc"
  y2v <- "firstIn"
  
  p <- plot_ly(dt, x = xv) %>%
    add_lines(y = y1v, name = "win_perc") %>%
    add_lines(y = y2v, name = "secondWon", visible = T) 
  
  
  p <- plot_ly(dt, x = ~tourney_date) %>%
    add_lines(y = ~win_perc, name = "win_perc") %>%
    add_lines(y = ~secondWon, name = "secondWon", visible = T) 
  
  p <- plot_ly(dt, x = ~tourney_date) %>%
    add_lines(y = ~win_perc, name = "win_perc") %>%
    add_lines(y = ~secondWon, name = "secondWon", visible = T) 
  
  %>%
    layout(
      title = "Drop down menus - Styling",
      xaxis = list(domain = c(0.1, 1)),
      yaxis = list(title = "y"),
      updatemenus = list(
        list(
          y = 0.8,
          buttons = list(
            
            list(method = "restyle",
                 args = list("line.color", "blue"),
                 label = "Blue"),
            
            list(method = "restyle",
                 args = list("line.color", "red"),
                 label = "Red"))),
        
        list(
          y = 0.7,
          buttons = list(
            list(method = "restyle",
                 args = list("visible", list(TRUE, FALSE)),
                 label = "Sin"),
            
            list(method = "restyle",
                 args = list("visible", list(FALSE, TRUE)),
                 label = "Cos")))
      )
    )
  
  
  
  
  
  
  
}

