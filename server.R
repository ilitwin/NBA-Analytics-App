#master
library(shiny)
library(plotly)
library(stringr)
library(ggplot2)
library(fiftystater)
library(rsconnect)
data("fifty_states")

p17 = read.csv("player2017.csv")
names(p17)[4] = "Total Rebounds"
season.avg <- read.csv("seasonavg.csv")
dat = read.csv("g1-final.csv")
dat1 = read.csv("nba.final.csv")
dat2 = read.csv("nba.final.by.college.csv")

#age
nba1 = read.csv(file = "player_data.csv")
nba1 = nba1[is.na(nba1$birth_date)==F, ]
nba1 = nba1[is.na(nba1$year_end)==F, ]
nba1$birth_date = as.vector(nba1$birth_date)
nba1$age = as.numeric(nba1$year_end)
for (i in 1:length(nba1$birth_date)) {
  w = as.character(nba1$birth_date[i])
  l = nchar(w)
  nba1$age[i] = as.numeric(nba1$year_end[i]) - as.numeric(substr(w, (l-3), l))
}

nba = nba1[,c('name', 'position', 'birth_date', 'year_end')]
nba = nba[is.na(nba$birth_date)==F, ]
nba = nba[is.na(nba$year_end)==F, ]
nba$birth_date = as.vector(nba$birth_date)

for (i in 1:length(nba$birth_date)) {
  w = as.character(nba$birth_date[i])
  l = nchar(w)
  nba$birth_date[i] = as.numeric(substr(w, (l-3), l))
}

nba$age = as.numeric(nba$year_end) - as.numeric(nba$birth_date)
nba$position = as.character(nba$position)
nba = nba[nba$position != "", ]
nba$position = str_to_lower(nba$position)

names(nba1) = c("Name", "year_start", "End year", "Position", "Height", "Weight", "Birth date", 
                "College", "Age")


#MAP
nba2 <- read.csv(file = "Players.csv")

new.df <- nba2[,c('Player', 'born', 'height', 'weight', 'birth_city', 'birth_state')]
new.df$birth_state = as.vector(new.df$birth_state)
new.df <- new.df[new.df$birth_state != "", ]
new.df$birth_state = str_to_lower(new.df$birth_state)
new.df$birth_city = str_to_lower(new.df$birth_city)
names(new.df)[5] = "subregion"
names(new.df)[6] = "region"
new.df$ones <- rep(1, nrow(new.df))

m.weight = function(x) {
  mean(x$weight)
}
m.height = function(x) {
  mean(x$height)
}
s.players = function(x) {
  sum(x$ones)
}

df = ddply(new.df, .(region), m.weight)
df5 = ddply(new.df, .(region), m.height)
df6 = ddply(new.df, .(region), s.players)
df$mean.height = df5[,c("V1")]
df$sum.players = df6[,c("V1")]
names(df)[2] = 'mean.weight'

df = df[df$region %in% unique(fifty_states$id),] # US state-level data
abb = setNames(state.abb, state.name)
df$code = state.abb[match(df$region, str_to_lower(state.name))]

df$hover <- with(df, paste(str_to_title(region), '<br>', "Mean weight (kg)", round(mean.weight,2), '<br>', "Mean height (cm)", round(mean.height,2), "<br>"))  # TEXT in squares
# give state boundaries a white border
k <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
m <- list(
  l = 0,
  r = 1,
  b = 1,
  t = 0,
  pad = 4
)

t <- list(
  l = 1,
  r = 1,
  b = 1,
  t = 6,
  pad = 4
)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

pal = c("wheat", "tomato")
p.map <- plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(
    z = ~sum.players, text = ~hover, locations = ~code, 
    marker = list(line = k),
    color = ~sum.players, colors = pal
  ) %>%
  colorbar(title = "Number of Players", thinckness = 5,
           len =.3, ticks ="", outlinewidth = 0.0001, xanchor = "center",
           bgcolor = "white") %>%
  layout(
    geo = g, autosize = F, width = 600, height = 300, 
    margin = m
  )

col.names1 = c("Player","Year","Games","Game.Started","Minutes.Played","Player.Efficiency.Rating",
               "True.Shooting","Free.Throw.Rate",
               "Offensive.Rebound.Percentage", "Defensive.Rebound.Percentage",
               "Total.Rebound.Percentage","Assist.Percentage", 
               "Steal.Percentage","Block.Percentage",
               "Turnover.Percentage","Usage.Percentage",
               "Offensive.Win.Shares","Defensive.Win.Shares",
               "Win.Shares","Win.Shares.Per.48.Minutes",
               "Box.Plus.Minus", "Value.Over.Replacement",
               "Field.Goals","Field.Goal.Attempts","Field.Goal.Percentage",
               "Three.Point.Field.Goals","Three.Point.Field.Goal.Attempts",
               "Three.Point.Field.Goal.Percentage",
               "Two.Point.Field.Goals","Two.Point.Field.Goal.Attempts",  
               "Two.Point.Field.Goal.Percentage",
               "Effective.Field.Goal.Percentage",
               "Free.Throws","Free.Throw.Attempts","Free.Throw.Percentage",
               "Offensive.Rebounds","Defensive.Rebounds","Total.Rebounds",
               "Assists","Steals","Blocks","Turnovers",
               "Personal.Fouls","Points")

col.names = c("Games","Game.Started","Minutes.Played","Player.Efficiency.Rating",
              "True.Shooting","Free.Throw.Rate",
              "Offensive.Rebound.Percentage", "Defensive.Rebound.Percentage",
              "Total.Rebound.Percentage","Assist.Percentage", 
              "Steal.Percentage","Block.Percentage",
              "Turnover.Percentage","Usage.Percentage",
              "Offensive.Win.Shares","Defensive.Win.Shares",
              "Win.Shares","Win.Shares.Per.48.Minutes",
              "Box.Plus.Minus", "Value.Over.Replacement",
              "Field.Goals","Field.Goal.Attempts","Field.Goal.Percentage",
              "Three.Point.Field.Goals","Three.Point.Field.Goal.Attempts",
              "Three.Point.Field.Goal.Percentage",
              "Two.Point.Field.Goals","Two.Point.Field.Goal.Attempts",  
              "Two.Point.Field.Goal.Percentage",
              "Effective.Field.Goal.Percentage",
              "Free.Throws","Free.Throw.Attempts","Free.Throw.Percentage",
              "Offensive.Rebounds","Defensive.Rebounds","Total.Rebounds",
              "Assists","Steals","Blocks","Turnovers",
              "Personal.Fouls","Points")

seasonstats = read.csv("Seasons_Stats.csv")

topfive = seasonstats[seasonstats$Player %in% c("Kareem Abdul-Jabbar*", "Karl Malone*", "Kobe Bryant", "Michael Jordan*", "Wilt Chamberlain*"),]

toptens = read.csv("toptens.csv")
compilation = read.csv("compilation.csv")

yearz = unique(toptens$Year)
statz = c("PTS", "AST", "TRB", "STL", "BLK")

library(tm)
library(wordcloud)
library(memoise)
library(plyr)

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(year, sta, dat) {
  if (!(year %in% yearz))
    stop("No data for this year")
  if (!(sta %in% statz))
    stop("No data for this statistic")
  
  newtab = na.omit(dat[dat$Year == year, c("Player", sta)])
  newvec = as.vector(newtab[,2])
  n = as.vector(newtab[,1])
  
  names(newvec) = n
  sort(newvec, decreasing = TRUE)
  
})

function(input, output, session) {
  #time stat
  output$s.a.Plot <- renderPlotly({
    
    plot_ly(y = season.avg[,input$Statistic], x = season.avg$Year, 
            type = "bar", color = "orange") %>%
      layout(yaxis = list(title = 'Average Player Statistic'), 
             xaxis = list(title = 'Year'))
    
  })
  
  #pie chart
  output$plot.pie <- renderPlot({
    
    ptemp <- data.frame(stat = colnames(p17)[c(4:8)], 
                        Total = as.numeric(p17[p17$Player == as.character(input$name),c(4:8)]))
    p <- ggplot(ptemp, aes(x=" ", y = Total, fill = stat)) + 
      geom_bar(stat = "identity") + coord_polar("y", start = 0) +
      labs(xlab = as.character(input$name),
           ylab = " ", 
           fill = "Statistic") + theme_minimal() + scale_fill_brewer(palette="Set1") +
      theme(axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank())
    
    print(p)
  })
  
  #age
  d <- reactive({
    dist <- switch(input$dist,
                   "f-c" = nba[nba$position == "f-c", ]$age,
                   "c-f" = nba[nba$position == "c-f", ]$age,
                   "c" = nba[nba$position == "c", ]$age,
                   "g" = nba[nba$position == "g", ]$age,
                   "f" = nba[nba$position == "f", ]$age,
                   "g-f" = nba[nba$position == "g-f", ]$age,
                   "f-g" = nba[nba$position == "f-g", ]$age,
                   "f-c")
  })
  vec = c("Name", "Birth date", "End year", "Age", "Height", "Weight")
  h <- reactive({
    dist <- switch(input$dist,
                   "f-c" = na.omit(nba1[nba1$Position == "F-C", vec]),
                   "c-f" = na.omit(nba1[nba1$Position == "C-F", vec]),
                   "c" = na.omit(nba1[nba1$Position == "C", vec]),
                   "g" = na.omit(nba1[nba1$Position == "G", vec]),
                   "f" = na.omit(nba1[nba1$Position == "F", vec]),
                   "g-f" = na.omit(nba1[nba1$Position == "G-F", vec]),
                   "f-g" = na.omit(nba1[nba1$Position == "F-G", vec]),
                   "f-c")
  })
  
  output$plot <- renderPlotly({
    dist <- input$dist
    
    plot_ly(x = d(), type = "histogram", 
            main = str_to_upper(input$dist),
            color = "orange", border = "white",
            xlab = "Age",
            breaks = 20) %>%
      layout(yaxis = list(title = 'Number of Players'), 
             xaxis = list(title = 'Age'))
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    h()
  })
  
  #MAP
  output$plot.map <- renderPlotly({
    p.map
  })
  
  #COLLEGE VS YEAR
  output$Plot.1 <- renderPlotly({
    
    # Render a barplot
    tab = table(dat[dat$college == input$college,]$year_end -
                  dat[dat$college == input$college,]$year_start)
    nd = as.vector(tab[order(tab)])
    plot_ly(y = nd, x = names(nd), type = "bar",
            main=input$college, color = "orange") %>%
      layout(yaxis = list(title = 'Number of Players'), 
             xaxis = list(title = 'Number of Years'))
  })
  
  # NBA Player Statistics Trend
  selectedData <- reactive({
    dat1[dat1$Player == input$xcol, input$ycol]
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5, 4, 0, 1))
    plot(selectedData(),
         xlab = "Seasons Played", ylab = "Statistic",
         ylim = range(dat1[dat1$Player == input$xcol,input$ycol]),
         pch = 20, cex = 3, col = "orange1")
    lines(selectedData())
    
  })
  
  
  # MDS
  output$team.plot <- renderPlot({
    cont.cols.y = which(names(dat2) %in% col.names1)
    plot.dat.y = dat2[dat2$college == input$s1, cont.cols.y]
    
    cont.cols = which(names(dat2) %in% col.names)
    plot.dat = dat2[dat2$college == input$s1, cont.cols]
    
    plot.dat.scaled = scale(plot.dat)
    dist.plot.dat = dist(plot.dat.scaled)
    plot.dat.mds <- cmdscale(dist.plot.dat, k = 2)
    colnames(plot.dat.mds) <- c("mds.coordinate.1", "mds.coordinate.2")
    plot.dat.y <- cbind(plot.dat.y, plot.dat.mds)
    
    p <- ggplot(plot.dat.y, aes(mds.coordinate.1, mds.coordinate.2)) +
      geom_point(aes(color=factor(dat2[dat2$college ==input$s1,]$Player)), 
                 size=input$pt.size)+
      labs(x = "MDS Coordinate 1", y = "MDS Coordinate 2",
           color = "Player") +
      theme_minimal() 
    p
  })
  
  output$fiveplot <- renderPlotly({
    plot_ly(x = topfive[,input$statzzz], y = topfive$PER, type = "scatter", 
            mode = 'markers', text = ~paste('Player: ', topfive$Player), 
            marker = list(color = factor(topfive$Player, 
                                         labels = c("orange", "purple","blue", 
                                                    "red", "green")))) %>%
      layout(yaxis = list(title = 'PER'), 
             xaxis = list(title = input$statzzz))
    
  })
    
    #word map
    termz <- reactive({
      # Change when the "update" button is pressed...
      input$update
      # ...but not for anything else
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          getTermMatrix(input$selection, input$selection2, compilation)
        })
      })
    })
    
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot.c2 <- renderPlot({
      v <- termz()
      if (input$selection2 == "PTS"){
        wordcloud_rep(names(v), v, scale=c(3, 0.5),
                      colors=brewer.pal(10, "Paired"))  
      }
      else{
        wordcloud_rep(names(v), v, scale=c(4, 0.5),
                      colors=brewer.pal(10, "Paired"))
        }
      }, width = 550, height = 600) 


}


