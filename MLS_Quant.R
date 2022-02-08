# Quantitative Analyst Assessment
# Author: Jameel Kaba

# Libraries
library(dplyr)
library(skellam)
library(ggplot2)

set.seed(10) 

# 1.1 Prepare a dataset containing the results of MLS games since the start of the 2012 season.
# 1.2 Add two columns called “home_conference” and “away_conference” with the respective team conferences

# Load data
data <- read.csv('Insert data here', header = TRUE)
data <- as.data.frame(data)

# Create Home conference and Away conference columns
data$Home_Conference <- rep('MLS', nrow(data))
data$Away_Conference <- rep('MLS', nrow(data))

# Home/Road goal differential
data$Home_GD <- rep(0, nrow(data))
data$Away_GD <- rep(0, nrow(data))

for(i in 1:nrow(data)){
  
  # Calculate Goal Differential
  data$Home_GD[i] <- data$HG[i] - data$AG[i]
  data$Away_GD[i] <- data$AG[i] - data$HG[i]
  
  # Setting Atlanta Utd and Atlanta United as the same team
  if(data$Home[i] == 'Atlanta Utd'){
    data$Home[i] <- 'Atlanta United'
  }
  
  # Setting conference to East
  if(data$Home[i] %in% c('Atlanta United', 'Chicago Fire', 'Columbus Crew', 'DC United', 'FC Cincinnati',
                         'Houston Dynamo', 'Inter Miami', 'Montreal Impact', 'Nashville SC', 'New England Revolution',
                         'New York City', 'New York Red Bulls', 'Orlando City', 'Philadelphia Union', 
                         'Sporting Kansas City', 'Toronto FC')){
    data$Home_Conference[i] <- 'East'
  }
 
  # Setting conference to West
  if(data$Home[i] %in% c('Chivas USA', 'Colorado Rapids', 'FC Dallas', 'Los Angeles FC', 'Los Angeles Galaxy',
                         'Minnesota United', 'Portland Timbers', 'Real Salt Lake', 'San Jose Earthquakes',
                         'Seattle Sounders', 'Vancouver Whitecaps')){
    data$Home_Conference[i] <- 'West'
  }
  
  # Conference Swap occured after the 2014 season
  if(data$Season[i] > 2014 && (data$Home[i] == 'Sporting Kansas City' || data$Home[i] == 'Houston Dynamo')){
    data$Home_Conference[i] <- 'West'
  }
  
  # Repeat conference setup for Away
  if(data$Away[i] == 'Atlanta Utd'){
    data$Away[i] <- 'Atlanta United'
  }
  
  if(data$Away[i] %in% c('Atlanta United', 'Chicago Fire', 'Columbus Crew', 'DC United', 'FC Cincinnati', 
                         'Houston Dynamo', 'Inter Miami', 'Montreal Impact', 'Nashville SC', 'New England Revolution', 
                         'New York City', 'New York Red Bulls', 'Orlando City', 'Philadelphia Union', 
                         'Sporting Kansas City', 'Toronto FC')){
    data$Away_Conference[i] <- 'East'
  }
  
  if(data$Away[i] %in% c('Chivas USA', 'Colorado Rapids', 'FC Dallas', 'Los Angeles FC', 'Los Angeles Galaxy', 
                         'Minnesota United', 'Portland Timbers', 'Real Salt Lake', 'San Jose Earthquakes', 
                         'Seattle Sounders', 'Vancouver Whitecaps')){
    data$Away_Conference[i] <- 'West'
  }
  
  if(data$Season[i] >2014 && (data$Away[i] == 'Sporting Kansas City' || data$Away[i] == 'Houston Dynamo')){
    data$Away_Conference[i] <- 'West'
  }
}

# Putting date into proper format
data$Date <- as.Date(data$Date, "%d/%m/%Y")

# Assign Home teams a number
data$HomeNum <- factor(data$Home)
data$HomeNum <- as.numeric(data$HomeNum)

# Assign Away teams a number
data$AwayNum <- factor(data$Away)
data$AwayNum <- as.numeric(data$AwayNum)

# Number of teams in data
nt <- length(unique(data$Home))

# 2.1 What is the percentage of games in the fitset where teams are in different conferences?

# Filter the set of data
fitset <- subset(data, Date <= as.Date("2015-12-06"))
simset <- subset(data, Date > as.Date("2015-12-06") & Date <= as.Date("2016-10-23"))

# Number of games between teams of same conference in fitset
s <- 0
for(i in 1:nrow(fitset)){
  if(fitset$Home_Conference[i] == fitset$Away_Conference[i]){
    s <- s+1
  }
}

# Proportion of games between teams of differenct conferences in fitset
diff_prop <- 1 - (s/nrow(fitset))
diff_prop

fit_east <- subset(fitset, Home_Conference == "East" & Away_Conference == "East")
fit_west <- subset(fitset, Home_Conference == "West" & Away_Conference == "West")
fit_same<-subset(fitset,(Home_Conference == "East" & Away_Conference == "East") | 
                   (Home_Conference == "West" & Away_Conference == "West"))

# 2.2 Is the home advantage for the Eastern Conference different from the Western Conference?

# Using home goal differences in each conference to look at the difference in mean home side advantage
t.test(fit_east$Home_GD, fit_west$Home_GD, paired=FALSE, alternative="two.sided")

# 2.3 Can you spot any seasonality in the total goals scored? 

# Moving average column
fit_same$TG_MA <- rep('N/A', nrow(fit_same))

# Moving average over 'X' number of games
n <- 20 

for(i in n:row(fit_same)){
  MA <- 0
  
  for(j in (i-n+1):i){
    if(fit_same$Season[i-n+1] == fit_same$Season[i]){
      MA <- MA + fit_same$HG[j] + fit_same$AG[j]
    }
    
    # These NAs are ok for periods at the start of seasons
    else{
      MA <- 'N/A'
    }
  }
  fit_same$TG_MA[i] <- MA
}
fit_same$TG_MA <- as.numeric(fit_same$TG_MA)

# Plotting total goals moving average for the fitsame dataset
MovingAverages <- data.frame(Date = fit_same$Date, Moving_Average = fit_same$TG_MA)
ggplot(data = MovingAverages, mapping = aes(x = Date, y = Moving_Average)) +
  geom_line(color = 'blue') +
  ggtitle('20 Game Total Goals Moving Average Over Time') + 
  xlab('Date') + ylab('20 Game Total Goals Moving Average') +
  theme_grey(base_size = 12) + theme(plot.title = element_text(hjust = 0.5))

# 3.2 Assuming games are independent of each other, fit this model by maximum likelihood on the fitset

InvLogit = function(x){
  "Inverse Logit function, used to constrain inputs between 0 and 1"
  return(exp(x) / (1 + exp(x)))
}

Model_LL <- function(P, data){
  "This gives the negative model log likelihood.
  P is the vector formodel parameters, before being constrained by InvLogit.
  Data is the fitted data to be used, containing home and away goals."
  
  # Constraining the initial model parameters inputted
  P = InvLogit(P)
  
  # Alpha values for each team
  A = P[1:nt]
  
  # Beta values for each team
  B = -1 * P[(nt+1):(2*nt)]
  
  # Gamma Value
  G = P[(2*nt + 1)]
  
  # Eta Value
  E = P[(2*nt + 2)]
  
  # Log likelihood
  LL <- 0
  
  for(i in 1:nrow(data)){
    lambda <- exp(A[data$HomeNum[i]] + B[data$AwayNum[i]] + G + E/2)
    mu <- exp(A[data$AwayNum[i]] + B[data$HomeNum[i]] + G - E/2)
    
    LL <- LL + (-lambda - log(factorial(data$HG[i])) + data$HG[i] * log(lambda)) + 
      (-mu - log(factorial(data$AG[i])) + data$AG[i] * log(mu))
  }
  # Negative log likelihood outputted. Need to minimize for best fit
  return(-LL)
}

# Fitting the model parameters by minimizing the negative log likelihood
fit = optim(c(rep(2, nt), rep(2, nt), 2, 2), Model_LL, data = fit_same, method = "BFGS",
            control = list(maxit = 10000, REPORT = 1, trace = 1, fnscale = 100), hessian = FALSE)

# Vector of fitted model parameters
OUT = InvLogit(fit$par)

# Fitted alpha values for each team
A = OUT[1:nt]

# Fitted beta values for each team
B = -1 * OUT[(nt+1):(2*nt)]

# Fitted gamma value
G = OUT[(2*nt+1)]

# Fitted eta value
E = OUT[(2*nt+2)]

# Number of teams in fitsame
Team <- unique(fit_same$Home)
nt_f <- length(Team)

# Column  of alpha values for each team
Alpha <- rep(0, nt_f)

# Column of negative beta values for each team
Neg_Beta <- rep(0, nt_f)

# Alpha rankings for each team 
Alpha_Rank <- rep(0, nt_f)

# Negative Beta rankings for each team
Neg_Beta_Rank <- rep(0, nt_f)

for(i in 1:nt_f){
  Alpha[i] <- A[subset(fit_same, Home == Team[i])$HomeNum[1]]
  Neg_Beta[i] <- -1 * B[subset(fit_same, Home == Team[i])$HomeNum[1]]
}

for(i in 1:nt_f){
  h <- 0
  for(j in 1:nt_f){
    if(Alpha[j] > Alpha[i]){
      h <- h + 1
    }
  }
  Alpha_Rank[i] <- h + 1
}

for(i in 1:nt_f){
  h <- 0
  for(j in 1:nt_f){
    if(Neg_Beta[j] > Neg_Beta[i]){
      h <- h + 1
    }
  }
  Neg_Beta_Rank[i] <- h + 1
}

# Ordered alpha & negative beta table for each team with rankings
Params_Table <- data.frame(Teanm, Alpha, Alpha_Rank, Neg_Beta, Neg_Beta_Rank)
Params_Table <- Params_Table %>% arrange(Team)

# 5.1 Compute the scoring rates λ and μ for each game in the simulation set.
# 5.2 Compute the probability of home win, draw and home lose for each game in the simulation set.

# Lambdas for games in simset
simset$lambda <- rep(0, nrow(simset))

# Mus for games in simset
simset$mu <- rep(0, nrow(simset))

# Probabilities of home wins
simset$phome <- rep(0, nrow(simset))

# Probabilities of draws
simset$pdraw <- rep(0, nrow(simset))

# Probabilties of away wins
simset$paway <- rep(0, nrow(simset))

for(i in 1:nrow(simset)){
  simset$lambda[i] <- exp(A[simset$HomeNum[i]] + B[simset$AwayNum[i]] + G + E/2)
  simset$mu[i] <- exp(A[simset$AwayNum[i]] + B[simset$HomeNum[i]] + G - E/2)
  
  #Skellam distribution to calculate probabilities
  simset$pdraw[i] = dskellam(0, simset$lambda[i], simset$mu[i])
  simset$paway[i] = dskellam(-1, simset$lambda[i], simset$mu[i])
  simset$phome[i] = 1 - simset$pdraw[i] - simset$paway[i]
}

# 5.3 Compute the expected table based on expected points of each team in each league on October 23rd 2016

# Number of teams in the simset
Team <- unique(simset$Home)
nt_s <- length(Team)
Conference <- rep('MLS', nt_s)

# Assign conference
for(i in 1:nt_s){
  if(Team[i] %in% c('Atlanta United', 'Chicago Fire', 'Columbus Crew', 'DC United', 'FC Cincinnati', 
                    'Inter Miami', 'Montreal Impact', 'Nashville SC', 'New England Revolution', 'New York City', 
                    'New York Red Bulls', 'Orlando City', 'Philadelphia Union', 'Toronto FC')){
    Conference[i] <- 'East'
  }
  if(Team[i] %in% c('Chivas USA', 'Colorado Rapids', 'FC Dallas', 'Los Angeles FC', 'Los Angeles Galaxy', 
                    'Minnesota United', 'Portland Timbers', 'Real Salt Lake', 'San Jose Earthquakes', 
                    'Seattle Sounders', 'Vancouver Whitecaps', 'Sporting Kansas City', 'Houston Dynamo')){
    Conference[i] <- 'West'
  }
}

# Expected overall table
Points <- rep(0, nt_s)
Table <- data.frame(Conference, Team, Points)

for(i in 1:nt_s){
  p <- 0
  
  for(j in 1:nt_s){
    # Expected drawand win points added for home teams
    if(simset$Home[j] == Table$Team[i]){
      p <- p + simset$pdraw[j] + simset$phone[j] * 3
    }
    
    # Expected draw and win points added for away teams
    if(simset$Away[j] == Table$Team[i]){
      p <- p + simset$pdraw[j] + simset$paway[j] * 3
    }
  }
  Table$Points[i] = p
}

# Expected overall, eastern, and western tables arranged by points
Exp_Table_Overall <- Table %>% arrange(desc(Points))
Exp_Table_East <- subset(Exp_Table_Overall, Conference == 'East')
Exp_Table_West <- subset(Exp_Table_Overall, Conference == 'West')

# 5.4 Suppose the predictions provided to you come from model A. 
#     Compare the predictive performance of the fitted model with model A.

# Import Model A data
A_data <- read.csv('Model_A', header = TRUE)
A_data <- as.data.frame(A_data)

# Home/Road Goals + Results added to A_Data
A_data$HG <- rep(0, nrow(A_data))
A_data$AG <- rep(0, nrow(A_data))
A_data$Res <- rep('MLS', nrow(A_date))

# Adding HG, AG, Res columns by matching vs simset
# While both data sets contain the same games, the order is different
for(i in 1:nrow(A_data)){
  for(j in 1:nrow(simset)){
    if(A_data$Date[i] == simset$Date[j] && (A_data$Home[i] == simset$Home[j]) && (A_data$Away[i] == simset$Away[j])){
      A_data$HG[i] <- simset$HG[j]
      A_data$AG[i] <- simset$AG[j]
      A_data$Res[i] <- simset$Res[j]
    }
  }
}

RPS = function(X, phome, pdraw){
  "Ranked probability score function 
  for results X are the actual results
  phome, pdraw, and probabilities on a home win and draw"
  
  # Number of games
  n <- length(X)
  score <- 0
  
  for(i in 1:n){
    # Identifiers for home win and draw
    z1 <- 0
    z2 <- 0
    
    if(X[i] == "H"){
      z1 <- 1
    }
    if(X[i] == "D"){
      z2 <- 1
    }
    score <- score + (z1 - phome[i])^2 + ((z1 - phome[i]) + (z2 - pdraw[i]))^2
  }
  RES <- score / (2*n)
  return(RES)
}

RPS_TG = function(HG, AG, EHG, EAG){
  "Ranked probability score function
  HG and AG are the actual home/away goals
  EHG and EAG are the expected home/away goals"
  
  # Number of games
  n <- length(HG)
  score <- 0
  
  for(i in 1:n){
    
    # Identifiers/Probabilities for number of goals up to a max of 20
    z <- rep(0,20)
    p <- rep(0,20)
    z[(HG[i] + AG[i])] <- 1
    
    for(j in 1:20){
      p[j] = dpois(j, (EHG[i] + EAG[i]))
    }
    
    for(j in 1:20){
      s <- 0
      for(k in 1:j){
        s <- s + (z[k] - p[k])
      }
      score <- score + s^2
    }
  }
  RES <- score / (20*n)
  return(RES)
}

# RPS for fitted model and model A for match results & total goals
RPS_fit <- RPS(simset$Res, simset$phome, simset$pdraw)
RPS_A <- RPS(A_data$Res, A_data$expected_team1_win, A_data$expected_draw)
RPS_TG_fit <- RPS_TG(simset$HG, simset$AG, simset$lambda, simset$mu)
RPS_TG_A <- RPS_TG(A_data$HG, A_data$AG, A_data$expected_team1_goals, A_data$expected_team2_goals)

# 6 Write some code that simulates the games from March 6th 2016 to October 23rd 2016 using the 
#     models estimates and distribution.

sim_table <- function(simset){
  "This creates a simultated table, fitted lambda and mu values are used"
  TableOut <- list()
  
  # Simulated home/away goals & results
  simset$sim_HG <- rep(0, nrow(simset))
  simset$sim_AG <- rep(0, nrow(simset))
  simset$sim_Res <- rep('MLS', nrow(simset))
  
  Team <- unique(simset$Home)
  Conference <- rep('Conf', length(Team))
  
  # Assign conferences
  
  for(i in 1:length(Team)){
    if(Team[i] %in% c('Atlanta United', 'Chicago Fire', 'Columbus Crew', 'DC United', 'FC Cincinnati', 
                      'Inter Miami', 'Montreal Impact', 'Nashville SC', 'New England Revolution', 'New York City', 
                      'New York Red Bulls', 'Orlando City', 'Philadelphia Union', 'Toronto FC')){
      Conference[i] <- 'East'
    }
    if(Team[i] %in% c('Chivas USA', 'Colorado Rapids', 'FC Dallas', 'Los Angeles FC', 'Los Angeles Galaxy', 
                      'Minnesota United', 'Portland Timbers', 'Real Salt Lake', 'San Jose Earthquakes', 
                      'Seattle Sounders', 'Vancouver Whitecaps', 'Sporting Kansas City', 'Houston Dynamo')){
      Conference[i] <- 'West'
    }
  }
  
  # Resulting points, goal differential, goals scored
  Points <- rep(0, length(Team))
  GD <- rep(0, length(Team))
  GS <- rep(0, length(Team))
  Table <- data.frame(Conference, Team, Points, GD, GS)
  
  # Match Simultations
  for(i in 1:nrow(simset)){
    simset$sim_HG[i] <- rpois(1, simset$lambda[i])
    simset$sim_AG[i] <- rpois(1, simset$mu[i])
    
    if(simset$sim_HG[i] > simset$sim_AG[i]){
      simset$sim_Res[i] <- "H"
    }
    
    if(simset$sim_HG[i] == simset$sim_AG[i]){
      simset$sim_Res[i] <- "D"
    }
    
    if(simset$sim_AG[i] > simset$sim_HG[i]){
      simset$sim_Res[i] <- "A"
    }
  }
  # Populating table columns
  for(i in 1:nrow(simset)){
    for(j in 1:nrow(Table)){
      if(simset$Home[i] == Table$Team[j]){
        if(simset$sim_Res[i] == "H"){
          Table$Points[j] <- Table$Points[j] + 3
        }
        if(simset$sim_Res[i] == "D"){
          Table$Points[j] <- Table$Points[j] + 1
        }
        Table$GD[j] <- Table$GD[j] + (simset$sim_HG[i] - simset$sim_AG[i])
        Table$GS[j] <- Table$GS[j] + simset$sim_HG[i]
      }
      if(simset$Away[i] == Table$Team[j]){
        if(simset$sim_Res[i] == "A"){
          Table$Points[j] <- Table$Points[j] + 3
        }
        if(simset$sim_Res[i] == "D"){
          Table$Points[j] <- Table$Points[j] + 1
        }
        Table$GD[j] <- Table$GD[j] + (simset$sim_AG[i] - simset$sim_HG[i])
        Table$GS[j] <- Table$GS[j] + simset$sim_AG[i]
      }
    }
  }
  TableOut$Overall <- Table %>% arrange(desc(Points), desc(GD), desc(GS), Team)
  TableOut$East <- subset(TableOut$Overall, Conference == 'East')
  TableOut$West <- subset(TableOut$Overall, Conference == 'West')
  
  return(TableOut)
}

# Single Table Simulation
sim_1 <- sim_table(simset)
