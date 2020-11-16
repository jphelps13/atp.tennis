min_date <- "2013-01-01"

fdt <- dt[date >= min_date]

fdt <- fdt[, max_rank := min(ranking), by = player_id]
fdt <- fdt[max_rank <= 5]


pdt <-  fdt[, point_var := cumsum(point_change) , 
            by = .(simple_name)]

pdt[, point_var := point_var - point_var[date == min(date)], by = simple_name]

pls <- sort(unique(pdt$simple_name))

N <- length(pls)
sam <- sample(pls, N)

sdt <- pdt[simple_name %in% sam,]

cols <- rep(c("#9c9c9c"), 30)[1:N]


f <- sdt[, diff(range(point_var)), by = simple_name]
f <- f[order(V1)]

select <- c("novak_djokovic", "roger_federer", "andy_murray",
            "milos_raonic", "david_ferrer", "rafael_nadal")
new_cols <- c("#DC3737", "#D24F44", "#C96751", "#C07F5F",
              "#B7976C", "#AEAF7A")
new_cols <- rep("#D24F44", 6)
cols[match(select, pls)] <- new_cols[]

size_dt <- pdt[, .(simple_name = unique(simple_name))]
size_dt <- size_dt[, size := 0]
size_dt[simple_name %in% select, size := 1]
setkey(sdt, simple_name)
setkey(size_dt, simple_name)
sdt <- size_dt[sdt]

# main
p <- ggplot(data = sdt) + 
  geom_line(aes(x = date, y = point_var, group = simple_name,
                color = simple_name, size = size))+
  scale_color_manual(values=cols) + scale_size(range=c(0.1,1), guide=FALSE) 
p

p <- removeGgplotGrid(p)
p
ff <- subset(sdt, date == max(date))
p <- p + geom_text(data = ff, 
                   aes(label = simple_name, x = date, y = point_var), hjust = -.1) + 
  theme(legend.position="none")
p

p <- p + theme_black()
p

# exploratory
p <- ggplot(data = sdt) + 
  geom_step(aes(x = date, y = point_var, group = simple_name,
                color = simple_name), size = 0.5)
p






theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "#9c9c9c", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "#9c9c9c", lineheight = 0.9),  
      axis.ticks = element_line(color = "#9c9c9c", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "#9c9c9c", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "#9c9c9c", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "#9c9c9c",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "#9c9c9c"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_blank(),  
      panel.border = element_blank(),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "#9c9c9c"),  
      strip.text.y = element_text(size = base_size*0.8, color = "#9c9c9c",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "#9c9c9c"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
}

removeGgplotGrid <- function(pl){
  pl <- pl +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  return(pl)
}


plot(sdt$date, sdt$point_var, col = "darkgrey")

