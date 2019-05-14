#########################################################################
# Parameters and Estimates
#########################################################################
#simulate a polling scenario
library(tidyverse)
library(dslabs)
ds_theme_set()
take_poll(25)

#calculate standard error of a poll of 25 people varying the proportion of democrats from 0 to all
# `N` represents the number of people polled
N <- 25
# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0,1,length.out = 100)
# Create a variable `se` that contains the standard error of each sample average
se <- sqrt(p*(1-p)/N)
# Plot `p` on the x-axis and `se` on the y-axis
plot(p,se)
#plots based on 3 sample sizes
sample_sizes <- c(25,100,1000)
for(i in sample_sizes){
  se <- sqrt(p*(1-p)/i)
  plot(p,se,ylim = c(0,max(se)))
}

# Write a function called `take_sample` that takes `p` and `N` as arguments and returns the average value of a randomly sampled population.
#1=democrat, 0=republican
take_sample <- function(p,N){
  x <- sample(0:1,N,c(1-p,p),replace = TRUE)
  mean(x)
}
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45
# Define `N` as the number of people polled
N <- 100
# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p,N)
# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications
B <- 10000
errors <- replicate(B, p - take_sample(p,N))
mean(errors)

#gray are in this chart represents confidence intervals
data("nhtemp")
data.frame(year=as.numeric(time(nhtemp)),temperature=as.numeric(nhtemp)) %>%
  ggplot(aes(year,temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")
#monte carlo simulation to verify that random interval contains the proportion 95% of the time
inside <- replicate(B, {
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat-2*SE_hat, X_hat+2*SE_hat) #check if random sample is between 2 standard errors more or less than of the estimated average
})
mean(inside)
#suppose a random sample of 100 is taken out of an urn red and blue beads results in getting 52 blue beads
#using the null hypothesis (assume that there is an equal amount of red and blue beads, aka the spread is zero)
#calculate the likelihood of drawing 52 blue beads if there was in fact in equal amount of beads
N <- 100
observed_spread <- 0.02
z <- sqrt(N)*observed_spread/.5
1 - (pnorm(z)-pnorm(-z)) #69% of time this value could occur if same amount of beads, meaning this sample does not indicate there are more blue beads

###calculate confidence intervals from polling data from 2016 election###
library(dslabs)
#assume only 2 candidates and construct a 95% confidence interval for the election night proportion
# Load the data
data(polls_us_election_2016)
# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls <- filter(polls_us_election_2016, state=="U.S." & enddate >= 2016-10-31)
# How many rows does `polls` contain? Print this value to the console.
nrow(polls)
# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
print(N)
# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat <- polls$rawpoll_clinton[1] / 100
print(X_hat)
# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N)
print(se_hat)
# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(X_hat-qnorm(.975)*se_hat,X_hat+qnorm(.975)*se_hat)
print(ci)
#create new dataset with these values for all poll results
polls <- mutate(polls, X_hat=rawpoll_clinton/100, se_hat=sqrt(X_hat*(1-X_hat)/samplesize), lower=X_hat-qnorm(.975)*se_hat, upper=X_hat+qnorm(.975)*se_hat)
pollster_results <- select(polls, pollster, enddate, X_hat, se_hat, lower, upper)
#add column that checks whether the election result for Clinton of 48.2% was in the confidence intervals of the polls
pollster_results <- mutate(pollster_results, hit=lower<=.482 & upper>=.482)
mean(pollster_results$hit)
# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.")  %>%
  mutate(d_hat = rawpoll_clinton/100 - rawpoll_trump/100)
# Assign the sample size of the first poll in `polls` to a variable called `N`.
N <- polls$samplesize[1]
# For the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`.
d_hat <- polls$d_hat[1]
# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat+1)/2
# Calculate the standard error of the spread and save it to a variable called `se_hat`
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(d_hat - qnorm(0.975)*se_hat, d_hat + qnorm(0.975)*se_hat)
# Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% mutate(hit = lower<=0.021 & upper>=0.021) %>% summarize(mean(hit))
# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster.
polls <- mutate(polls, error = d_hat-0.021)
polls %>% group_by(pollster) %>% filter(n() >= 5) %>%
  ggplot(aes(x = pollster, y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## simulate polls from just before 2012 election to see why Nate Silver reported 90% confidence that Obama would win
d <- 0.039 #spread of actual election results
Ns <- c(1298,533,1342,897,774,254,812,324,1291,1056,2172,516) #poll sample sizes
p <- (d+1)/2 #calculate proportion of people voting for Obama based on the spread
#monte carlo simulate results of polls
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1),size=N,replace=TRUE,prob=c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat,X_hat-2*SE_hat,X_hat+2*SE_hat)-1
})
polls <- data.frame(poll=1:ncol(confidence_intervals), t(confidence_intervals), sample_size=Ns)
names(polls) <- c("poll","estimate","low","high","sample_size")
polls
#most polls include zero in the interval, so it would have to be called a toss up, but if these results are aggregated the intervals can be narrowed
d_hat <- polls %>% summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>% .$avg
p_hat <- (1+d_hat)/2 #calc proportion of Obama voters based on aggregate spread
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size)) #calc margin of error
#predict results
round(d_hat*100,1)
round(moe*100,1)
#review results of actual polls in 2016 election by aggregating "quality" polls
polls <- polls_us_election_2016 %>% filter(state=="U.S." & enddate>="2016-10-31" & (grade %in% c("A+","A","A-","B+") | is.na(grade)))
polls <- polls %>% mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
d_hat <- polls %>% summarize(d_hat = sum(spread*samplesize) / sum(samplesize)) %>% .$d_hat #calc the aggregated spread
p_hat <- (d_hat+1)/2 #calc proportion for Clinton
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize)) #calc margin of error
round(d_hat*100,2)
round(moe*100,2)
#actual result was 2.1% which is outside of margin of error, how did this go wrong?
#look at a histogram of the polling results, this does not look like a normal distribution and the standard error is more than .0066
polls %>% ggplot(aes(spread)) + geom_histogram(color="black",binwidth=.01)
#to see why, look at the pollsters who are conducting several polls
polls %>% group_by(pollster) %>% filter(n()>=6) %>% ggplot(aes(pollster,spread)) + geom_point() + theme(axis.text.x = element_text(angle=90,hjust=1))
#the above plot shows that one pollster has Trump winning by 4% and another Clinton by over 5%, so must be flaws in poll questions/methodology, aka pollster effect
#let's work with the last polls taken by each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>% filter(enddate==max(enddate)) %>% ungroup()
one_poll_per_pollster %>% ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)
#instead of working with the spread of the voting results, instead look at spread of the pollsters
sd(one_poll_per_pollster$spread)
results <- one_poll_per_pollster %>% summarize(avg=mean(spread),se=sd(spread)/sqrt(length(spread))) %>% mutate(start=avg-1.96*se, end=avg+1.96*se)
round(results*100,1)

#We have been using urn models to motivate the use of probability models. However, most data science applications are not related to data obtained from urns. 
#More common are data that come from individuals. Probability plays a role because the data come from a random sample.
#The random sample is taken from a population and the urn serves as an analogy for the population.
#Let's revisit the heights dataset.

# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)
# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height
#calc the population average
mu <- mean(x)
#take a sample of 50 heights
N <- 50
X <- sample(x,N,replace = TRUE)
X_hat <- mean(X)
se <- sd(X)/sqrt(N)
ci <- c(X_hat - qnorm(0.975)*se, X_hat + qnorm(0.975)*se)
#monte carlo of confidence intervals
B <- 10000
res <- replicate(B, {
  X <- sample(x,N,replace = TRUE)
  X_hat <- mean(X)
  se <- sd(X)/sqrt(N)
  interval <- c(X_hat - qnorm(0.975)*se, X_hat + qnorm(0.975)*se)
  between(mu, interval[1], interval[2])
})
mean(res)
#we used visualization to motivate the presence of pollster bias in election polls. Here we will examine that bias more rigorously. 
#Lets consider two pollsters that conducted daily polls and look at national polls for the month before the election. Is there poll bias? Let's visualize
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 
polls %>% ggplot(aes(spread,pollster)) + geom_boxplot() + geom_point()
#estimate the standard errors for the two pollsters
sigma <- polls %>% group_by(pollster) %>% summarize(s = sd(spread))
# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 
# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate <- res$avg[2] - res$avg[1]
# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(estimate - qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)
# Calculate the p-value
z <- estimate/se_hat
(1 - pnorm(z)) * 2
#Note that our data has more than two pollsters. We can also test for pollster effect using all pollsters, not just two.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-15" & state == "U.S.") %>%  group_by(pollster) %>%
  filter(n() >= 5) %>%   mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>% ungroup()
# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation.
var <- polls %>% group_by(pollster) %>% summarize(avg=mean(spread),s=sd(spread))

### Bayesian Statistics ###
# Bayes Theorem -> Probability of A happening given B happening is Pr(A and B) / Pr(B) or Pr(B given A) * Pr(A) / Pr(B)

#monte carlo to demonstrate Bayes Theorem - randomly select 100000 and test whether they have cystic fibrosis (1 in 3900 chance, test is 99% accurate)
#Bayes theorem calculates out to a 2% chance that if you test positive you will actually have the disease, code below proves that
prev <- 1/3900  #chance of having the disease
N <- 100000  #number of people to sample
outcome <- sample(c("Disease","Healthy"), N, replace=TRUE, prob=(c(prev,1-prev)))  #random sampling of 100000
N_D <- sum(outcome=="Disease")  #number of people with disease
N_H <- sum(outcome=="Healthy")  #number of people without disease
#so, because the chance of having the disease is so low there is a higher number of people who will falsely test positive than you would think
accuracy <- 0.99 #accuracy of test
test <- vector("character",N)  #vector to store results
test[outcome=="Disease"] <- sample(c("+","-"), N_D, replace=TRUE, prob=c(accuracy,1-accuracy))  #random sample based on test accuracy of the diseased population 
test[outcome=="Healthy"] <- sample(c("-","+"), N_H, replace=TRUE, prob=c(accuracy,1-accuracy))  #random sample based on test accuracy of the healthy population
table(outcome,test)  #see the large number of healthy people who tested positive but don't have it
sum(test[outcome=="Disease"]=="+")/(sum(test[outcome=="Disease"]=="+") + sum(test[outcome=="Healthy"]=="+")) #probability of having disease after positive test

#Jose Iglesias started his first season batting .450 (9 of 20). The batting averages for players of the 3 prior years was .275 with a standard error of .027.
#Is Jose the next coming of Ted Williams?
#we can estimate his average based on observed data

###election forecasting case study###
#reset everything up for prior examples
library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%
  ungroup()
results <- one_poll_per_pollster %>% 
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>% 
  mutate(start = avg - 1.96*se, end = avg + 1.96*se) 
#use bayesian approach to forecast 2016 election
mu <- 0 #set mu to 0 meaning we have no indication who would win (this would normally have some type of value based on "fundamentals")
tau <- 0.035 #this is the average difference in popular vote
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
#calc a credible interval
posterior_mean + c(-1.96,1.96)*posterior_se
#calc a probability that Clinton will win popular vote (i.e. d > 0)
1 - pnorm(0,posterior_mean,posterior_se)
#this is too high and doesn't align with five-thirty-eight's prediction of 81.6% because we are not accounting for pollster bias (variability in polling data)
#let's say we have 5 pollsters with 6 polls with a total of 2000 samples and the spread average (d) is 2.1%, we can simulate 6 data points from these polls
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J,0,2*sqrt(p*(1-p)/N))
})
#but sampled data doesn't match actual data because the model isn't accounting for pollster to pollster variability yet
# To fix this, we add a new term for the pollster effect to represent the house effect of the i-th pollster, this will be represented by h with an assumed 
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I,0,.025)
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J,0,2*sqrt(p*(1-p)/N))
})
#if we go back to our original forecast and add bias variability (approx .025 based on prior election years), we get a more realistic number
mu <- 0 
tau <- 0.035 
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
posterior_mean + c(-1.96,1.96)*posterior_se
1 - pnorm(0,posterior_mean,posterior_se)
#let's look at electoral college forecasting rather than popular vote since that is what decides who wins
#look at polls by state
results <- polls_us_election_2016 %>%
  filter(state!="U.S." & !str_detect(state, "CD") & enddate >="2016-10-31" & (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))
results %>% arrange(abs(avg)) #order by closest margin
results <- left_join(results, results_us_election_2016, by="state") #add the election results to the dataset
results <- mutate(results, sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd)) #for states with only 1 poll, add a standard deviation
#use a monte carlo to simulate election results using the bayesian model
B <- 10000
mu <- 0
tau <- 0.02
clinton_EV <- replicate(B, {
  results %>% mutate(sigma = sd/sqrt(n), 
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B * mu + (1 - B) * avg,
                     posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>% #add the electoral college votes for Clinton
    summarize(clinton = sum(clinton)) %>% 
    pull(clinton) + 7  #add the votes from RI and DC where no polls were collected as they were guaranteed to be Clinton
})
mean(clinton_EV > 269)
#almost 100% change Clinton will win, but this is not accounting for poll bias, add that in and run again
bias_sd <- 0.03
clinton_EV_2 <- replicate(B, {
  results %>% mutate(sigma = sqrt(sd^2/n + bias_sd^2),
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B * mu + (1 - B) * avg,
                     posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>% #add the electoral college votes for Clinton
    summarize(clinton = sum(clinton)) %>% 
    pull(clinton) + 7  #add the votes from RI and DC where no polls were collected as they were guaranteed to be Clinton
})
mean(clinton_EV_2 > 269)

### Exercises ####
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
# Create an object called `cis` that has the columns indicated in the instructions
cis <- polls %>% mutate(X_hat = (spread+1)/2, 
                        se = 2*sqrt(X_hat*(1-X_hat)/samplesize),
                        lower = X_hat-qnorm(.975)*se,
                        upper = X_hat+qnorm(.975)*se) %>%
  select(state,startdate,enddate,pollster,grade,spread,lower,upper)
# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
p_hits <- cis %>% mutate(hit=actual_spread>lower & actual_spread<upper) %>% group_by(pollster) %>% filter(length(spread)>=5) %>% 
  summarize(proportion_hits=mean(hit), n=n(), grade=grade[1]) %>% arrange(proportion_hits)
p_hits %>% ggplot(aes(x=pollster,y=proportion_hits)) + geom_bar(stat = "identity") + coord_flip()


# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls
p_hits <- errors %>% group_by(state) %>% filter(n()>=5) %>% summarize(proportion_hits=mean(hit)) %>% mutate(n=n()) %>% arrange(proportion_hits)
# Make a barplot of the proportion of hits for each state
p_hits %>% ggplot(aes(state,proportion_hits)) + geom_bar(stat = "identity") + coord_flip()

### Association Tests ###
#is there bias for funding awards between men and women in the Netherlands
#main evidence comes from comparing percentages, but would random variance explain the difference
library(tidyverse)
library(dslabs)
data("research_funding_rates")
research_funding_rates %>% 
  select(discipline, contains("total"))
#view the values available in the data
names(research_funding_rates)
#compute the totals that were successful and not
totals <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(list(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women)
totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                     percent_women = yes_women/(yes_women+no_women))
#what would we see if funding were random
funding_rate <- totals %>% summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men +yes_women + no_women)) %>% pull(percent_total)
funding_rate
#so is there bias? chi-square test can answer this
#first step is to have a 2-by-2 table
two_by_two <- data.frame(awarded=c("no","yes"),men=c(totals$no_men,totals$yes_men),women=c(totals$no_women,totals$yes_women))
two_by_two
#general idea of the Chi-square test is to compare this two-by-two table to what you expect to see
data.frame(awarded = c("no", "yes"), 
           men = (totals$no_men + totals$yes_men) * c(1 - funding_rate, funding_rate), 
           women = (totals$no_women + totals$yes_women) * c(1 - funding_rate, funding_rate))
#this shows more men were funded than expected, and less women, but is this just random variability, the chi-square test can tell us the likelihood of a deviation this large
two_by_two %>% select(-awarded) %>% chisq.test()
#this low p-value indicates there was bias