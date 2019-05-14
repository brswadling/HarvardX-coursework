#####################################################
# R Basics, Functions, and Data Types
#####################################################

#assign a value to a variable
a <- 4.5
#brings package into memory
library(dslabs)
#brings dataset into memory
data("murders")
#shows the object type (data.frame in this case)
class(murders)
#shows the structure of the object (data.frame will show number of rows, column variable names and types)
str(murders)
#shows the first 6 rows of a dataset
head(murders)
#shows the column names of a dataset
names(murders)
#prints the data in the column population of dataset murders
murders$population
#create a new vector of just the population column and display it
pop <- murders$population
print(pop)
pop        #shortcut to printing

######################################################
# Vectors, Sorting
######################################################

#create vectors using concatenate/combine function and then associating the 2 vectors
codes <- c(380,124,818)
country <- c("italy","canada","egypt")
names(codes) <- country
print(codes)
#alternate way to create codes vector by assigning titles to numbers directly
codes <- c(italy=380,canada=124,egypt=818)
print(codes)

#sequence function to generate list of numbers in a range (optional 3rd argument denotes increment to count by)
#this could be used to assign to a vector a seqence of numbers
seq(1,10)   #1 thru 10
seq(1,10,2) #odd numbers
seq(2,10,2) #even numbers
#use length function to find out number of entries
x <- seq(4,100,3)
length(x)
#shortcut to getting a sequence of numbers
1:10

#subsetting to access specific parts of a vector, use []
codes[2]
codes[c(1,3)] #get more than one using a multi-entry vector
codes[1:2] #use a sequence to get a sequential subset
#access entry directly using name
codes["canada"]
codes[c("egypt","italy")]

#using coersion to assign different types to same vector
x <- c(1,"canada",3)
x        #shows quotes around all 3 entries
class(x) #shows that vector was creates as a character string
#explicit coercion functions
x <- 1:5
y <- as.character(x) #change numbers to characters
as.numeric(y) #change back to numbers and display (does NOT change what y is storing)

#missing data is displayed as NA (can occur in coercion if it can't make the conversion)
x <- c("1","b","3")
as.numeric(x) #can't convert b to a number so gives warning message that an NA was introduced

#sorting contents of a vector
x <- c(31,4,15,92,65)
x
sort(x) #will display the values in asending order
#order function creates a list of the indexes of the values of the vector in ascending order
order(x)
#sort the states by largest number of gun murders
library(dslabs)
data("murders")
index <- order(murders$total) #create a vector of the indexes from least to most murders
murders$abb[index] #display states based on that vector
#get the largest and smallest values in a vector
max(murders$total)
i_max <- which.max(murders$total) #get the index of the largest murder count
murders$state[i_max]
min(murders$total)
i_min <- which.min(murders$total)
murders$state[i_min]
#find the relative ranked value of each item in a vector against the others
rank(murders$total) #i.e. California would be 50 and Vermont 1

# Define a variable states to be the state names from the murders data frame
states <- murders$state
# Define a variable ranks to determine the population size ranks 
ranks <- rank(murders$population)
# Define a variable ind to store the indexes needed to order the population values
ind <- order(murders$population)
# Create a data frame my_df with the state name and its rank and ordered from least populous to most 
my_df <- data.frame(state = states[ind], pop_rank = ranks[ind])

# Using new dataset 
library(dslabs)
data(na_example)
# Checking the structure 
str(na_example)
# Find out the mean of the entire dataset 
mean(na_example)
# Use is.na to create a logical index ind that tells which entries are NA
ind <- is.na(na_example)
# Determine how many NA ind has using the sum function
sum(ind)

# Note what we can do with the ! operator
x <- c(1, 2, 3)
ind <- c(FALSE, TRUE, FALSE)
x[!ind]
# Create the ind vector
library(dslabs)
data(na_example)
ind <- is.na(na_example)
# We saw that this gives an NA
mean(na_example)
# Compute the average, for entries of na_example that are not NA 
mean(na_example[!ind])

#arithmetic functions done to a vector object are done to each element in the vector
heights <- c(69,62,66,70,70,73,67,73,67,70) #heights in inches
heights * 2.54 #converts inches to centimeters
avg = mean(heights) #find average of the heights
heights - avg #find how far from the average each of the heights is
#find the murder rate per 100,000 people
murder_rate <- murders$total / murders$population * 100000
murders$state[order(murder_rate,decreasing=TRUE)]
# Calculate the average murder rate in the US 
mean(murder_rate)

# Assign city names to `city` 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
# Store temperature values in `temp`
temp <- c(35, 88, 42, 84, 81, 30)
# Convert temperature into Celsius and overwrite the original values of 'temp' with these Celsius values
temp <- (temp-32) * 5/9
# Create a data frame `city_temps` 
city_temps <- data.frame(name=city,temperature=temp)

# Define an object `x` with the numbers 1 through 100
x <- 1:100
# Compute the sum 
sum(1/x^2)

###########################################################################
# Indexing, Data Wrangling, Plots
###########################################################################

library(dslabs)
data("murders")
murder_rate <- murders$total / murders$population * 100000
names(murder_rate) <- murders$state #add state name to the murder rate
#apply logical operators to items in a vector, find the states with murder rate of Italy or lower
index <- murder_rate <= 0.71
index
#show the state names using the index object
murders$state[index]
#find how many states have a murder rate equal or lower than Italy
sum(index)
#get list of states in west part of U.S. (want to be in mountains) and ones that are safer
west <- murders$region == "West"
safe <- murder_rate <= 1
index <- safe & west
murders$state[index]
#find the indices that are true
x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
which(x)
#find the murder rate of a specific state
index <- which(murders$state == "Massachusetts")
murder_rate[index]
#find matches from one vector to another
index <- match(c("New York","Florida","Texas"), murders$state)
murder_rate[index]
#want to know whether each value in one vector is in another vector
x <- c("a","b","c","d","e")
y <- c("a","d","f")
y %in% x
c("Boston","Dakota","Washington") %in% murders$state

#introduce dplyr package for more advanced data table manipulation
library(dplyr)
#add a new field in the murders dataset
murders <- mutate(murders,rate=total/population*100000)
head(murders)
#apply a filter to the dataset
filter(murders,rate<=0.71)
nrow(filter(murders,rate<=0.71)) #count of rows after filter applied
filter(murders,region %in% c("West","South"))
#use select to only get the fields you want
new_table <- select(murders,state,region,rate)
#chain functions together by piping results into the next function
murders %>% select(state,region,rate) %>% filter(rate <= 0.71)


#create a data frame
grades <- data.frame(names=c("John","Juan","Jean","Yao"),
                     exam1=c(95,80,90,85),
                     exam2=c(90,85,85,90),
                     stringsAsFactors = FALSE)

#visualize by plotting data points
population_in_millions<-murders$population/1000000
total_murders<-murders$total
plot(murders$population/1000000,murders$total)
#show a histogram to see the murder rates
hist(murders$rate)
murders$state[which.max(murders$rate)]
#use a boxplot to compare regions
boxplot(rate~region,data=murders)

#######################################################################
# Programming Basics
#######################################################################

library(dslabs)
data("murders")
murder_rate <- murders$total / murders$population * 100000
#basic syntax of if statement
ind <- which.min(murder_rate) #get smallest murder rate row
if(murder_rate[ind] < 0.5){
  print(murders$state[ind])
} else{
  print("No state has a murder rate below 0.5")
}
#any and all check vector values
if(any(murder_rate<0.5)){   #see if any values are below .5 then try .1
  print("There are safe places to live")
}else{
  print("Get a gun, there are no safe places to live")
}
if(all(murder_rate>1)){  #see if all values are 1, then try .1
  print("Get a gun, there are no safe places to live")
} else{
  print("There are safe places to live")
}

#ifelse function to apply conditional to all rows of a vector
data(na_example)
sum(is.na(na_example)) #145 NAs in dataset
no_nas <- ifelse(is.na(na_example),0,na_example) #replace NAs with zero
sum(is.na(no_nas)) #no NAs now

#create your own function that calculates an average (note: mean function is built into R and already does this)
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}
z <- 1:100
avg(z)
identical(mean(z),avg(z))
#function with 2 arguments (it calcs either arithmetic or geometric mean)
avg <- function(x, arithmetic=TRUE){
  n <- length(x)
  ifelse(arithmetic,sum(x)/n,prod(x)^(1/n))
}
avg(z,FALSE)

#create a summing function and then use a loop to calc for multiple values in a vector
compute_vector_sum <- function(n){
  x <- 1:n
  sum(x)
}
for(i in 1:25){
  print(compute_vector_sum(i))
}
#populate a vector with calculated sums
m <- 25
vector_sums <- vector(length = m) #create empty vector
for(i in 1:m){
  vector_sums[i] <- compute_vector_sum(i)
}
plot(vector_sums)
lines(i*(i+1)/2)
#for loops are rarely used, rather apply, sapply, tapply, mapply functions are used
