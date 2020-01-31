#master

library(shinythemes)
library(shiny)
library(plotly)
library(ggplot2)
library(fiftystater)
library(rsconnect)
if(!require(maps)) stop("This app requires the maps package.")
if(!require(mapproj)) stop("This app requires the maps package.")

data("fifty_states")

p17 = read.csv("player2017.csv")
season.avg <- read.csv("seasonavg.csv")

dat = read.csv("g1-final.csv")
dat1 = read.csv("nba.final.csv")
dat2 = read.csv("nba.final.by.college.csv")
dat.c = read.csv("nba.csv")
seasonstats = read.csv("Seasons_Stats.csv")
players = read.csv("player_data.csv")

topten = seasonstats[seasonstats$Player %in% c("Kareem Abdul-Jabbar*", "Karl Malone*", "Kobe Bryant", "Michael Jordan*", "Wilt Chamberlain*"),]

toptenp = topten[1,c("Year", "Player", "PTS")]
toptens = topten[1,c("Year", "Player", "STL")]
toptenb = topten[1,c("Year", "Player", "BLK")]
toptenr = topten[1,c("Year", "Player", "TRB")]
toptena = topten[1,c("Year", "Player", "AST")]

yearz <- unique(topten$Year)

for (i in yearz){
  asdf = seasonstats[seasonstats$Year == i,]
  p = order(-asdf$PTS)[1:10]
  s = order(-asdf$STL)[1:10]
  b = order(-asdf$BLK)[1:10]
  r = order(-asdf$TRB)[1:10]
  a = order(-asdf$AST)[1:10]
  
  toptenp = rbind(toptenp, asdf[p,c("Year", "Player", "PTS")])
  toptens = rbind(toptens, asdf[s,c("Year", "Player", "STL")])
  toptenb = rbind(toptenb, asdf[b,c("Year", "Player", "BLK")])
  toptenr = rbind(toptenr, asdf[r,c("Year", "Player", "TRB")])
  toptena = rbind(toptena, asdf[a,c("Year", "Player", "AST")])
}

toptenp = toptenp[-1,]
toptens = toptens[-1,]
toptenb = toptenb[-1,]
toptenr = toptenr[-1,]
toptena = toptena[-1,]

toptenp = na.omit(toptenp)
toptens = na.omit(toptens)
toptenb = na.omit(toptenb)
toptenr = na.omit(toptenr)
toptena = na.omit(toptena)

toptenp$STL = NA
toptenp$BLK = NA
toptenp$TRB = NA
toptenp$AST = NA

toptena$PTS = NA
toptena$STL = NA
toptena$BLK = NA
toptena$TRB = NA

toptens$PTS = NA
toptens$AST = NA
toptens$BLK = NA
toptens$TRB = NA

toptenb$PTS = NA
toptenb$AST = NA
toptenb$STL = NA
toptenb$TRB = NA

toptenr$PTS = NA
toptenr$AST = NA
toptenr$STL = NA
toptenr$BLK = NA

compilation = rbind(toptenp, toptens, toptenb, toptenr, toptena)
compilation = compilation[compilation$Year >= 1974,]

yearz = unique(toptens$Year)
statz = c("Points" = "PTS", "Assists" = "AST", "Total Rebounds"  = "TRB", "Steals" = "STL", "Blocks" = "BLK")

library(tm)
library(wordcloud)
library(memoise)
library(plyr)

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(year, sta) {
  if (!(year %in% yearz))
    stop("No data for this year")
  if (!(sta %in% statz))
    stop("No data for this statistic")
  
  newtab = na.omit(compilation[compilation$Year == year, c("Player", sta)])
  newvec = as.vector(newtab[,2])
  n = as.vector(newtab[,1])
  
  names(newvec) = n
  sort(newvec, decreasing = TRUE)
  
})



fluidPage(theme = shinytheme("united"),

  tags$head(
            tags$style(HTML("
                            @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                            
                            h1 {
                            font-family: 'Lobster', cursive;
                            font-weight: 500;
                            line-height: 1.1;
                            color: #48ca3b;
                            }
                            
                            "))
            ),
          
  titlePanel(HTML('<h1 class="display-3"><font color="grey">Lemon Nose</font></h1>')),
  
  navlistPanel(widths = c(3, 9),  

    #Intro   
    tabPanel(titlePanel(HTML("<h4>Introduction</h4>")),  
  HTML('<div class="jumbotron">
  <h1 class="display-3">Welcome to Our NBA App!</h1>
    <hr>
     <p class="lead">This app displays nine interactive graphs separated by the tabs on the left. The dataset that we used contains aggregate individual statistics for 67 NBA seasons starting from 1950. The data was obtained from the Basketball-reference website. </p>
     </p>
     <p>In the app, you can analyze this dataset from multiple aspects, such as players demographics, basic box-score attributes, such as points, assists, rebounds, and more advanced moneyball-like features, such as Value Over Replacement.</p>
     <p class="lead">
    </p>
    <p>Feel free to look around, and enjoy!<p>
    </p>
    <h2> <small class="text-muted">Brandon Hao, Christopher Hwang, Izabela Litwin, Huiwen Zhang</small> </h2>
    </p>
    <hr>
     <a class="btn btn-primary btn-lg" href="https://www.basketball-reference.com" role="button">Learn more</a>
     </p>
    </p>
    </p>
     </div>')
             
             # Generate a row with a sidebar
            
    ),
  
  #MAP
  tabPanel(titlePanel(HTML("<h4>Map of Player's Origin</h4>")),
           headerPanel(HTML("<h2>Map of Player's Origin</h2>")),
           sidebarPanel(helpText("This map shows the number of players in each state. Additionally, it displays average height and weight of the players in each state. Based on this map, we know that the largest number of NBA players was born in California. They also have above-average weights and heights.")),
           mainPanel(HTML("<h1>      </h1>"), HTML("<h1>      </h1>"), 
                     plotlyOutput("plot.map"))
  ),
  
  # Scatterplot
  tabPanel(titlePanel(HTML("<h4>Top 5 All-Time Scorers and PER</h4>")),
           headerPanel(HTML("<h2>Top 5 All-Time Scorers and PER</h4>")),
           sidebarLayout(      
             sidebarPanel(
               selectInput("statzzz", "Statistic:", 
                           choices=c("Points" = "PTS", "Assists" = "AST", "Total Rebounds" = "TRB")),
               hr(),
               helpText("This graph takes a look at the top 5 all-time leaders in points. The graph shows the relationship between their total points, assists, and rebounds per season and their player efficiency rating (PER) per season. We omitted steals and blocks as those two statistics were not recorded in Wilt Chamberlain's years of playing.")
             ),
             mainPanel(plotlyOutput("fiveplot"))
           )
  ),
  
  #Time stat data
  tabPanel(titlePanel(HTML("<h4>Average Statistics by Season</h4>")),
           headerPanel(HTML("<h2>Histogram of Average League Statistics</h2>")),
           
           # Generate a row with a sidebar
           sidebarLayout(      
             
             # Define the sidebar with one input
             sidebarPanel(
               selectInput("Statistic", "Statistic:", 
                           choices=c( "Field Goals" = "Field.Goals",
                                      "Field Goal Attempts" = "Field.Goal.Attempts",
                                      "Field Goal Percentage" = "Field.Goal.Percentage",
                                      "2 Point Field Goals" = "Two.Point.Field.Goals",
                                      "2 Point Field Goal Attempts" = "Two.Point.Field.Goal.Attempts",  
                                      "2 Point Field Goal Percentage" = "Two.Point.Field.Goal.Percentage",
                                      "Free Throws" = "Free.Throws",
                                      "Free Throw Attempts" = "Free.Throw.Attempts",
                                      "Free Throw Percentage" = "Free.Throw.Percentage",
                                      "Total Rebounds" = "Total.Rebounds",
                                      "Assists",
                                      "Personal Fouls" = "Personal.Fouls",
                                      "Points")),
               hr(),
               helpText("This histogram of various average player statistics per season shows how the league has evolved during the time period of 1950 - 2017.")
             ),
             
             # Create a spot for the barplot
             mainPanel(
               plotlyOutput("s.a.Plot")  
             )
             
           )
  ),
  
  tabPanel(titlePanel(HTML("<h4>Age at the End of Career</h4>")),
           headerPanel(HTML("<h2>Age at the End of Career</h2>")),
           
           # Sidebar layout with input and output definitions ----
           sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(
               
               # Input: Select the random distribution type ----
               radioButtons("dist", "Position type:",  # POSITON TYPE
                            c("F-C" = "f-c",
                              "C-F" = "c-f",
                              "C" = "c",
                              "G" = "g",
                              "F" = "f",
                              "G-F" = "g-f", 
                              "F-G" = "f-g")),
               
               hr(), 
               helpText("This graph examines the distribution of players' ages at the end of their careers, based on their positions. 
                        G stands for Guard, F for Forward, and C for Center. Positions with a dash show the primary position, then the secondary position. For example, F-C means a player is primarily a Forward, and secondarily a Center.")
               
               ),
             
             # Main panel for displaying outputs ----
             mainPanel(
               
               # Output: Tabset w/ plot, summary, and table ----
               tabsetPanel(type = "tabs",
                           tabPanel("Plot", plotlyOutput("plot")), 
                           tabPanel("Summary", verbatimTextOutput("summary")), 
                           tabPanel("Table", tableOutput("table"))
               )
               
             )
           )
           
           ),
    # 2017 pie chart    
    tabPanel(titlePanel(HTML("<h4>Average Player Contribution</h4>")),
             headerPanel(HTML("<h2>Pie Chart of Player Contribution Breakdown</h2>")), 
             
             # Generate a row with a sidebar
             sidebarLayout(      
               sidebarPanel( 
                 selectInput("name", "Player:", 
                             choices=p17[,1]),
                 hr(),
                 helpText("This breakdown of a player's contribution to his team during the 2017 season shows the orientation of each player's focus. For example, some are very focused on scoring (points), while others are role players (focused on rebounds etc).")
               ),
               
               # Create a spot for the barplot
               mainPanel(
                 plotOutput("plot.pie")
               )
               
             )
    ),

    # NBA Player Statistics Trend
    tabPanel(titlePanel(HTML("<h4>NBA Player Stats Trend</h4>")),
         headerPanel(HTML("<h2>NBA Player Statistics Trend</h2>")),
         sidebarLayout(
           sidebarPanel(
             selectInput('xcol', 'Select Player:', choices = unique(dat1$Player),
                         selectize = F),
             selectInput('ycol', 'Select Stats:', choices = c("Games",
                                                              "Game Started" = "Game.Started",
                                                              "Minutes Played" = "Minutes.Played",
                                                              "Player Efficiency Rating" = "Player.Efficiency.Rating",
                                                              "True Shooting" = "True.Shooting",
                                                              "Free Throw Rate" = "Free.Throw.Rate",
                                                              "Offensive Rebound Percentage" = "Offensive.Rebound.Percentage", 
                                                              "Defensive Rebound Percentage" = "Defensive.Rebound.Percentage",
                                                              "Total Rebound Percentage" = "Total.Rebound.Percentage",
                                                              "Assist Percentage" = "Assist.Percentage", 
                                                              "Steal Percentage" = "Steal.Percentage",
                                                              "Block Percentage" = "Block.Percentage",
                                                              "Turnover Percentage" = "Turnover.Percentage",
                                                              "Usage Percentage" = "Usage.Percentage",
                                                              "Offensive Win Shares" = "Offensive.Win.Shares",
                                                              "Defensive Win Shares" = "Defensive.Win.Shares",
                                                              "Win Shares" = "Win.Shares",
                                                              "Win Shares Per 48 Minutes" = "Win.Shares.Per.48.Minutes",
                                                              "Box Plus Minus" = "Box.Plus.Minus", 
                                                              "Value Over Replacement" = "Value.Over.Replacement",
                                                              "Field Goals" = "Field.Goals",
                                                              "Field Goal Attempts" = "Field.Goal.Attempts",
                                                              "Field Goal Percentage" = "Field.Goal.Percentage",
                                                              "3 Point Field Goals" = "Three.Point.Field.Goals",
                                                              "3 Point Field Goal Attempts" = "Three.Point.Field.Goal.Attempts",
                                                              "3 Point Field Goal Percentage" = "Three.Point.Field.Goal.Percentage",
                                                              "2 Point Field Goals" = "Two.Point.Field.Goals",
                                                              "2 Point Field Goal Attempts" = "Two.Point.Field.Goal.Attempts",  
                                                              "2 Point Field Goal Percentage" = "Two.Point.Field.Goal.Percentage",
                                                              "Effective Field Goal Percentage" = "Effective.Field.Goal.Percentage",
                                                              "Free Throws" = "Free.Throws",
                                                              "Free Throw Attempts" = "Free.Throw.Attempts",
                                                              "Free Throw Percentage" = "Free.Throw.Percentage",
                                                              "Offensive Rebounds" = "Offensive.Rebounds",
                                                              "Defensive Rebounds" = "Defensive.Rebounds",
                                                              "Total Rebounds" = "Total.Rebounds",
                                                              "Assists",
                                                              "Steals",
                                                              "Blocks",
                                                              "Turnovers",
                                                              "Personal Fouls" = "Personal.Fouls",
                                                              "Points"),
                         selected=names(dat1)[52], selectize = F),
             hr(),
             helpText("This is the summary of various types of statistics of each player over all the seasons. Via selecting the player and the statistic, you can see the trend of that statistic for that player throughout all the seasons he played.")
             
           ),
           mainPanel(
             plotOutput('plot1', width = 600, height = 400)
           )
         )
    ),

    #College vs years
    tabPanel(titlePanel(HTML("<h4>Years Players Played by College</H4>")),
             headerPanel(HTML("<h2>Years NBA Players Played by College</h2>")),
             
             # Generate a row with a sidebar
             sidebarLayout(  
               
               # Define the sidebar with one input
               sidebarPanel(
                 selectInput("college", "Select College:", 
                             choices=unique(dat$college),
                             selected = "Duke University"),
                 hr(),
                 helpText("This is the summary of the numbers of years NBA players played by college. Via selecting the college you are interested in, you can see the distribution of the years players from this college played.")
                 ),
 
               # Create a spot for the barplot
               mainPanel(
                 plotlyOutput("Plot.1")  
               )
               
             )
    ),


    #MDS
    tabPanel(titlePanel(HTML("<h4>MDS of Players Stats</h4>")),
             headerPanel(HTML("<h2>MDS of Players Stats</h2>")),
             sidebarPanel(
               sliderInput("pt.size", label = "Point Size:",
                           min = 1, max = 5, value = 3, step = 0.5),
               selectInput('s1', 'College:', choices = unique(dat2$college),
                           selected="Iona College"), 
               hr(), 
               helpText("This Multi-Dimensional Scaling (MDS) graph projects the original high-dimensional data into 2-dimensional plane which allows us to visualize the distances between each NBA player's records. The distance is calculated by scaling all the statistics. So, if two points having the same color are close to each other, then this player had similar performance in his two seasons; if two points having different colors are close to each other, then two different players had similar performance for one of their seasons.")
             ),
             mainPanel(
             plotOutput("team.plot", width = 600, height = 380) )
    ),

    # Top 10
    tabPanel(titlePanel(HTML("<h4>Top 10 Players</h4>")),
             headerPanel(HTML("<h2>Top 10 Players</h2>")),
         
         sidebarLayout(
           # Sidebar with a slider and selection inputs
           sidebarPanel(
             selectInput("selection", "Choose a year:",
                         choices = yearz),
             selectInput("selection2", "Choose a statistic:",
                         choices = statz),
             actionButton("update", "Change"),
             hr(), 
             helpText("This graph shows the top 10 players for a season starting at a chosen year given the statistic that you are interested in. The sizes and colors represent the rankings. The higher ranking, the larger size and darker color.")

           ),
           # Show Word Cloud
           mainPanel(
             plotOutput("plot.c2")
           )
         )
         )
    
  )

)
