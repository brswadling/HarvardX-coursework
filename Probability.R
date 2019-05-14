####################################################################
# Discrete Probablility
####################################################################

#simulate scenario of choosing a bead from a bowl, 2 red beads, 3 blue beads
beads <- rep(c("red","blue"), times = c(2,3)) #create an object to represent the red and blue beads in a bowl
sample(beads,2) #select two beads out of the bowl without replacing (replace = TRUE to replace bead)
events <- replicate(20000, sample(beads,1)) #simulate selecting a bead 20,000 times
table(events) #view the results
prop.table(table(events)) #view proportions
events <- sample(beads, 20000, replace = TRUE) #same as above replicate function call
prop.table(table(events)) #view proportions

#probabilities using a deck of cards
number <- "Three"
suit <- "Hearts"
paste(number, suit) #combines 2 strings
paste(letters[1:5], as.character(1:5)) #works on vectors also
expand.grid(pants = c("blue","black"), shirt = c("white","grey","plaid")) #show all combinations
#use all of this to generate a deck of cards
suits <- c("Clubs","Diamonds","Hearts","Spades")
numbers <- c("Ace","Deuce","Three","Four","Five","Six","Seven","Eight","Nine","Ten","Jack","Queen","King")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit)
#check probability of getting a king
kings <- paste("King",suits)
mean(deck %in% kings)
#permutations function computes, for any list of size n, all the different ways we can select r items
library(gtools)
permutations(5,2) 
permutations(10,7,v=0:9) #can use on a vector also
#draw 2 cards out of deck
hands <- permutations(52,2,v=deck)
nrow(hands) #total number of hand combinations
first_card <- hands[,1]
second_card <- hands[,2]
#probability of getting a king on second draw after getting a king on first draw
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)
#probability of getting dealt 2 kings
mean(deck %in% kings) * sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)
#likelihood of getting a natural 21 in blackjack - use combinations function since order doesn't matter
aces <- paste("Ace",suits)
facecards <- c("King","Queen","Jack","Ten")
facecards <- expand.grid(number=facecards, suit=suits)
facecards <- paste(facecards$number,facecards$suit)
hands <- combinations(52,2,v=deck)
mean(hands[,1] %in% aces & hands[,2] %in% facecards)
#same thing using a monte carlo simulation
results <- replicate(10000, {hand <- sample(deck,2)
                             (hand[1] %in% aces & hand[2] %in% facecards) | (hand[2] %in% aces & hand[1] %in% facecards)
                            }
                     )
mean(results)

#use monte carlo to find likelihood of 2 people in a room of 50 having the same birthday
people_in_room = 50
results <- replicate(10000, {
  bdays <- sample(1:365, people_in_room, replace = TRUE)
  any(duplicated(bdays)) #duplicated returns true or false if the number is appearing for second time, any checks for any trues
})
mean(results)
#rather than using static number of people in room, find percent chance based on group size
birthday_probability <- function(n, B=10000){  #create a function to do a montecarlo simulation using variable number of people (n)
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
n <- 1:60 #array with varying number of people in room
prob <- sapply(n, birthday_probability) #sapply allows a function to be applied to each element of a vector
library(dplyr)
library(ggplot2)
df <- data.frame(same_birthday_likelihood = prob, people_in_room = n)
df %>% ggplot(aes(people_in_room,same_birthday_likelihood)) + geom_point()
#rather than monte carlo, calculate the exact probability
exact_birthday_probability <- function(n){
  prob_unique <- seq(365,365-n+1)/365 #calculate the percent switch of not having the same birthday for each number in vector
  1 - prod(prob_unique) #multiply the vector elements to get the probability for that number of people n
}
eprob <- sapply(n, exact_birthday_probability)
df <- data.frame(same_birthday_likelihood = eprob, people_in_room = n)
df %>% ggplot(aes(people_in_room,same_birthday_likelihood)) + geom_line(col="red")
#figure out number of time to run a monte carlo simulation to get consistent probabilities
monte_carlo_probability <- function(B, n=22){ #function that runs variable number of simulations
  same_birthday <- replicate(B, {
    bdays <- sample(1:365,n,replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_birthday)
}
B <- 10 ^ seq(1,5,len=100) #vector of number 10 to 10000
simulator <- sapply(B, monte_carlo_probability) #test for variance in simulation runs
df <- data.frame(simulations=B,variance=simulator)
df %>% ggplot(aes(simulations,variance)) + geom_line() + scale_x_log10()

#The Celtics have a 60% chance of winning any individual game
# Calculate the frequency out of 10000 iterations that the Celtics won at least one game in a 4 game series.
# Create an object called `celtic_wins` that replicates two steps for B iterations: 
#(1) using the sample code to generate a four-game series `simulated_games`, then 
#(2) determining whether the simulated series contains at least one win for the Celtics.
celtics_wins <- replicate(10000, {
  results <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(results=="win")
  })
mean(celtics_wins)
#what is likelihood of Celtics winning all 4 games
celtics_wins <- replicate(10000, {
  results <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  !any(results=="lose")
})
mean(celtics_wins)

#replicate the game Let's Make a Deal where the contestant has picked one of three doors and the host has opened another door
#that shows a goat and then the contestant is asked whether they want to pick the other door or stay with their pick
#first, simulate staying with your pick
stick <- replicate(10000, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat")) #assign a prize to each door
  prize_door <- doors[prize=="car"] #find the door that has the prize behind it
  my_pick <- sample(doors,1) #simulate contestant picking 1 of the doors
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1) #simulate showing contestant one of the doors they didn't pick that has a goat
  stick <- my_pick #stay with original door selection
  stick == prize_door #did the contestant win
})
mean(stick)
#now, try changing the door picked
switch <- replicate(10000, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat")) #assign a prize to each door
  prize_door <- doors[prize=="car"] #find the door that has the prize behind it
  my_pick <- sample(doors,1) #simulate contestant picking 1 of the doors
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1) #simulate showing contestant one of the doors they didn't pick that has a goat
  switch <- doors[!doors %in% c(my_pick, show)]
  switch == prize_door #did the contestant win
})
mean(switch)

#Two teams, say the Cavs and the Warriors, are playing a seven game championship series. The first to win four games wins the series. 
#The teams are equally good, so they each have a 50-50 chance of winning each game.
#If the Cavs lose the first game, what is the probability that they win the series?

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes <- 0:1
# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`.
l <- replicate(6, list(outcomes))
# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities <- expand.grid(l)
# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
results <- rowSums(possibilities) >= 4
# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results)
#perform a monte carlo of same scenario
results <- replicate(10000, sum(sample(0:1,6, replace = TRUE)) >= 4)
mean(results)

####################################################################
# Continuous Probablility
####################################################################

#empirical cumulative distribution function (eCDF) example
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% .$height
F <- function(a) {mean(x<=a)}
hts <- seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE))
result <- sapply(hts,F)
result <- data.frame(height=hts,proportion=result)
result %>% ggplot(aes(height,proportion)) + geom_point()
#what is probability of a student randomly selected of being taller than 70.5 inches
1 - F(70)
#what is probability of a student being between 65 and 75 inches tall
F(75) - F(65)
#same questions using normal approximation
1 - pnorm(70.5,mean(x),sd(x))
pnorm(75,mean(x),sd(x)) - pnorm(65,mean(x),sd(x))

#monte carlo simulations using normal distribution
x <- heights %>% filter(sex=="Male") %>% .$height
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n,avg,s)
data.frame(simulated_heights=simulated_heights) %>% ggplot(aes(simulated_heights)) + geom_histogram(col="black",binwidth = 2)
#what is distribution of men over 7 feet tall
tallest <-replicate(10000, { 
  simulated_data <- rnorm(800,avg,s) #generate 800 heights
  max(simulated_data) #get the tallest height
})
mean(tallest >= 7*12) #what percent is 7 feet

#probability density
x <- seq(-4,4,length.out = 100)
data.frame(x,f=dnorm(x)) %>% ggplot(aes(x,f)) + geom_line()

#other distributions besides the normal are: student-t, chi-Squared, exponential, gamma, beta

####################################################################
# Random variables, sampling models, and the Central Limit Theorem
####################################################################

#random variable, note: statistics textbooks use capital letters to represent random variables
beads <- rep( c("red","blue"), c(2,3) )
X <- ifelse(sample(beads,1) == "blue", 1, 0)

#sampling models (most things can be represented as drawing from an urn)
#example: roulette betting on color (red is a win for player, black a loss)
color <- rep( c("black","red","green"), c(18,18,2) )
X <- sample(ifelse(color=="red",-1,1), 1000, replace = TRUE)
sum(X)
#better way is to use a sampling model, only 1 line of code
X <- sample(c(-1,1), 1000, replace = TRUE, prob = c(9/19, 10/19))
sum(X)
#simulate 1000 people play 10000 times and find out how often result was less than zero
S <- replicate(10000, sum(sample(c(-1,1), 1000, replace = TRUE, prob = c(9/19, 10/19))))
mean(S<=0)
#check for normal distribution
s <- seq(min(S),max(S),length=100)
normal_density <- data.frame(s=s, f=dnorm(s,mean(S),sd(S)))
data.frame(S=S) %>% ggplot(aes(S, ..density..)) +
  geom_histogram(color="black",binwidth=10) +
  ylab("Probability") +
  geom_line(data=normal_density, mapping=aes(s,f), color="blue")

#when the sample is large, the probability distribution is approximately normal, this is referred to as
#the Central Limit Theorem (CLT), which means all we need to describe a list of random values it the average and standard deviation

#calculate the average and standard deviation for roulette outcomes
n <- 1000
mu <- n * (20-18) / 38 #number of good outcomes - bad outcomes, divided by total possible outcomes
se <- sqrt(n) * abs(1-(-1)) * sqrt((20/38) * 18/38) #difference of max and min values, times square root of good outcomes times bad outcomes
pnorm(0,mu,se) #probability of less than 0 based on normal distribution

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)
# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- black+red / (green+black+red)
#Create a model to predict the random variable `X`, your winnings from betting on green.
X <- sample(c(-1,17), 1, replace=TRUE, prob=c(p_not_green,p_green))
# Print the value of `X` to the console
print(X)

###############################################################################################
# The Big Short
###############################################################################################

loss_per_foreclosure <- -200000
p <- .02 #percent of loans that default
n <- 1000 #number of 180,000 loans given out

#monte carlo of default rate
losses <- replicate(10000, {
  defaults <- sample( c(0,1), n, prob=c(1-p,p), replace=TRUE)
  sum(defaults * loss_per_foreclosure)
})
data.frame(losses_in_millions=losses/10^6) %>% ggplot(aes(losses_in_millions)) + 
  geom_histogram(binwidth=.6, col="black")

#CLT calculation instead of monte carlo

#take more risk
r <- .05 #interest rate
x <- r*180000
loss_per_foreclosure*p + x*(1-p)


# Assign the number of loans to the variable `n`
n <- 10000
# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000
# Assign the probability of default to the variable `p_default`
p_default <- 0.03
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Generate a vector called `defaults` that contains the default outcomes of `n` loans
defaults <- sample( c(0,1), n, prob=c(1-p_default,p_default), replace=TRUE )
# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
S <- sum(defaults) * loss_per_foreclosure


