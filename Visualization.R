###########################################################################
# Introduction to Data Visualization and Distributions
###########################################################################

library(dslabs)
data(heights)
names(heights)
head(heights)
#find the number of unique height entries
x <- unique(heights$height)
length(x)
#find frequency of values
tab <- table(heights$height)
#count number of entries that have a height of 1
sum(tab == 1)

#normal distribution
x <- 1:10
average <- sum(x) / length(x)
standard_deviation <- sqrt( sum((x-average)^2) / length(x) )
#normal distribution of Male heights using prebuilt functions
male_heights <- heights$height[heights$sex == "Male"]
average <- mean(male_heights)
stdev <- sd(male_heights)
#get the standard units for male heights
z <- scale(male_heights)
mean(abs(z)<2) #see how many heights are within 2 units of the average

#what proportion of the data is between 69 and 72 inches
y <- male_heights > 69 & male_heights <= 72 #returns true or false for each height
mean(y) #calculates average of 1(true) in proportion to the 0(false)
#approximate proportion without using vector x and only mean and standard deviation of x
pnorm(72,average,stdev) - pnorm(69,average,stdev)

#figure out what percent of all 7-footers are in the NBA (mean of 69 inches and stdev of 3 inches)
p <- 1 - pnorm(84,69,3) #proportion of 7-footers in population
seven_footers <- round(1000000000 * p) #approximately 1 billion men between ages 18 and 40
NBA_seven_footers <- 10 #there are 10 men in NBA over 7 feet
NBA_seven_footers / seven_footers #proportion of 7 footers in NBA

#quantiles are the values in data that represent specific proportions of the data
mean(male_heights <= 69.5) #69.5 is approximately the quantile for a .5 proportion
p <- seq(0.05, 0.95, 0.05) #create vector of quantiles in .05 increments
observed_quantiles <- quantile(male_heights,p) #quantiles based on the actual data
theoretical_quantiles <- qnorm(p,mean(male_heights),sd(male_heights)) #quantiles for a normal distribution
plot(theoretical_quantiles,observed_quantiles) #plot quantiles on simple graph
abline(0,1) #add a line to the graph

#compare male and female heights
female_heights <- heights$height[heights$sex=="Female"]
percentiles <- c(.1,.3,.5,.7,.9)
female_percentiles <- quantile(female_heights,percentiles)
male_percentiles <- quantile(male_heights,percentiles)
df <- data.frame(female = female_percentiles, male = male_percentiles)
print(df)

#the median and median absolution deviation are more robust measures as outliers do not affect them as much
#suppose a data entry error where a decimal was forgotton on an entry
female_heights_with_error <- female_heights
female_heights_with_error[1] <- female_heights_with_error[1] * 10
mean(female_heights_with_error) - mean(female_heights) #big difference
sd(female_heights_with_error) - sd(female_heights) #big difference
median(female_heights_with_error) - median(female_heights) #no difference
mad(female_heights_with_error) - mad(female_heights) #very minimal difference

#######################################################################################
# Introduction to ggplot2
#######################################################################################
#graphing murder data in a dotplot using ggplot2
library(ggplot2)
library(dslabs)
library(dplyr)
library(ggthemes)
library(ggrepel)
data("murders")
murder_rate <- sum(murders$total) / sum(murders$population) * 10^6
p <- ggplot(murders, aes(population_in_millions, total_murders, label = abb)) #specifies the x and y data for plotting
p + geom_abline(intercept = log10(murder_rate), lty = 2, color = "darkgrey") + #create a line with the average rate
  geom_point(aes(col=region), size = 3) + #specify a point plot and size of dots
  #geom_text(nudge_x = .05) + #add text to each point and move it to the right of the dot
  geom_text_repel() + #keeps text from being on top of each other
  scale_x_log10() + #change scale of x axis
  scale_y_log10() +  #change scale of y axis
  xlab("Population in millions") + #add x axis label
  ylab("Total number of murders") + #add y axis label
  ggtitle("US Gun Murders in US - 2010") + #add title to graph
  scale_color_discrete(name = "Region") + #change legend label
  theme_economist() #apply a theme

#graph male heights data
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
#histogram
p + geom_histogram(binwidth=1, fill="blue", col="black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")
#density
p + geom_density(fill="blue")
#q-q plots
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(sample = scale(height))) +
  geom_qq(dparams=params) +
  geom_abline()

#put plots next to each other to compare
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
p1 <- p + geom_histogram(binwidth=1, fill="blue", col="black")
p2 <- p + geom_histogram(binwidth=2, fill="blue", col="black")
p3 <- p + geom_histogram(binwidth=3, fill="blue", col="black")
library(gridExtra)
grid.arrange(p1,p2,p3, ncol=3)


#create group density plots for male and female heights
heights %>% ggplot(aes(height, fill=sex)) + geom_density(alpha=.2)

##################################################################################
# Summarizing with dplyr
##################################################################################
library(dplyr)
library(dslabs)
data(heights)
data(murders)

#use summarize function to create new table stored in a data frame that stores mean and standard deviation
# you can define the column names in the call (average and standard_deviation were chosen)
s <- heights %>% filter(sex=="Male") %>% summarize(average = mean(height),
                                                   standard_deviation = sd(height),
                                                   median = median(height),
                                                   minimum = min(height),
                                                   maximum = max(height))
s
s$average
s$standard_deviation

#calculate murder rate using summarize
us_murder_rate <- murders %>% summarize(rate = sum(total) / sum(population) * 100000)
us_murder_rate

#use the dot placeholder to return a numeric result rather than the table
us_murder_rate <- murders %>% summarize(rate = sum(total) / sum(population) * 100000) %>% .$rate
us_murder_rate

#summarize data by groups
s <- heights %>% group_by(sex) %>% summarize(average = mean(height),
                                             standard_deviation = sd(height),
                                             median = median(height),
                                             minimum = min(height),
                                             maximum = max(height))
s
murders <- mutate(murders,rate=total/population*100000) #add murder rate to data
murders %>% group_by(region) %>% summarize(median_rate = median(rate))

#sort and order tables
murders %>% arrange(population) %>% head() #sort by population
murders %>% arrange(rate) %>% head() #sort by murder rate
murders %>% arrange(desc(rate)) %>% head() #sort by murder rate descending
murders %>% arrange(region, rate) %>% head() #sort by region then murder rate
murders %>% arrange(desc(rate)) %>% top_n(10) #show top 10

#summarize function will return NA if any NAs in dataset, so remov NAs from a dataset
library(dslabs)
data("na_example")
mean(na_example)
sd(na_example)
mean(na_example, na.rm = TRUE)
sd(na_example, na.rm = TRUE)

#health dataset examples using dplyr
library(dplyr)
library(NHANES)
data("NHANES")
tab <- NHANES %>% filter(AgeDecade==" 20-29" & Gender=="female")
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% 
  summarize(average = mean(BPSysAve, na.rm=TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE))
ref_avg <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% 
  summarize(average = mean(BPSysAve, na.rm=TRUE)) %>% .$average
NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% 
  summarize(min = min(BPSysAve, na.rm=TRUE), max = max(BPSysAve, na.rm=TRUE))
NHANES %>% filter(Gender == "female") %>%
  group_by(AgeDecade) %>%
  summarize(average = mean(BPSysAve, na.rm=TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE))
NHANES %>% group_by(AgeDecade, Gender) %>%
  summarize(average = mean(BPSysAve, na.rm=TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE))
NHANES %>% filter(AgeDecade == " 40-49" & Gender == "male") %>% 
  group_by(Race1) %>%
  summarize(average = mean(BPSysAve, na.rm=TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>%
  arrange(average)

##############################################################################################
# Gapminder
##############################################################################################
library(dplyr)
library(ggplot2)
library(dslabs)
data("gapminder")
head(gapminder)
gapminder %>% arrange(desc(year)) #find the most recent year of data

#which country has a higher mortality rate
gapminder %>% filter(year==2015, country %in% c("Sri Lanka","Turkey")) %>% select(country,infant_mortality)
gapminder %>% filter(year==2015, country %in% c("Poland","South Korea")) %>% select(country,infant_mortality)
gapminder %>% filter(year==2015, country %in% c("Malaysia","Russia")) %>% select(country,infant_mortality)
gapminder %>% filter(year==2015, country %in% c("Pakistan","Vietnam")) %>% select(country,infant_mortality)
gapminder %>% filter(year==2015, country %in% c("Thailand","South Africa")) %>% select(country,infant_mortality)

#plot fertility rate against life expectancy
p <- gapminder %>% filter(year %in% c(1962,2012)) %>% ggplot(aes(fertility,life_expectancy))
p + geom_point(aes(col=continent)) +
  facet_grid(.~year) #compares 2 variables side by side, use a dot if only want one variable

#compare Europe to Asia by decade
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>% filter(year %in% years, continent %in% continents) %>% ggplot(aes(fertility,life_expectancy)) +
  geom_point(aes(col=continent)) + facet_wrap(.~year)

#look at fertility rates over time
gapminder %>% filter(country=="United States") %>% ggplot(aes(year,fertility)) +
  geom_line()
countries <- c("South Korea","Germany")
gapminder %>% filter(country %in% countries) %>% ggplot(aes(year,fertility,col=country)) +
  geom_line()
#look at life expectancy over time using labels instead of a legend
labels <- data.frame(country=countries, x=c(1975,1965), y=c(60,72))
gapminder %>% filter(country %in% countries) %>% ggplot(aes(year,life_expectancy,col=country)) +
  geom_line() +
  geom_text(data=labels, aes(x,y,label=country), size=5) +
  theme(legend.position="none")

#analyze income levels by region
past_year <- 1970
latest_year <- 2010
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365) #find the dollars per day a person lives on
gapminder %>% filter(year==past_year & !is.na(gdp)) %>% ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth = 1,col="black") +
  scale_x_continuous(trans="log2")
gapminder %>% filter(year==past_year & !is.na(gdp)) %>% 
  mutate(region = reorder(region,dollars_per_day,FUN=median)) %>%
  ggplot(aes(region, dollars_per_day,fill=continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  xlab("") +
  scale_y_continuous(trans="log2") +
  geom_point(show.legend = FALSE)
west <- c("Western Europe","Northern Europe","Southern Europe","Northern America","Australia and New Zealand")
#compare across past to present using only a common set of countries
west <- c("Western Europe","Northern Europe","Southern Europe","Northern America","Australia and New Zealand")
past_countries <- gapminder %>% filter(year==past_year,!is.na(gdp)) %>% .$country
latest_countries <- gapminder %>% filter(year==latest_year,!is.na(gdp)) %>% .$country
country_list <- intersect(past_countries,latest_countries)
gapminder %>% filter(year %in% c(past_year,latest_year) & !is.na(gdp) & country %in% country_list) %>%
  mutate(group=ifelse(region %in% west,"West","Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1,color="black") +
  scale_x_continuous(trans="log2") +
  facet_grid(year ~ group)
#compare side-by-side changes in region
gapminder %>% filter(year %in% c(past_year,latest_year) & !is.na(gdp) & country %in% country_list) %>%
  mutate(region = reorder(region,dollars_per_day,FUN=median)) %>%
  ggplot() +
  geom_boxplot(aes(region, dollars_per_day,fill=factor(year))) +
  theme(axis.text.x = element_text(angle=90,hjust = 1)) +
  xlab("") +
  scale_y_continuous(trans = "log2")
#use smooth density plot to show change in developing countries
gapminder %>% filter(year %in% c(past_year,latest_year) & !is.na(gdp) & country %in% country_list) %>%
  mutate(group=ifelse(region %in% west,"West","Developing")) %>%
  ggplot(aes(dollars_per_day, y=..count.., fill=group)) +  #differentiate between sizes of group using ..count.. variable found in geom_density function
  geom_density(alpha=.2,bw=.75) +
  scale_x_continuous(trans="log2") +
  facet_grid(year ~ .)  
#create more detailed groupings and show on smooth density plot
gapminder <- gapminder %>% 
  mutate(group = case_when(.$region %in% west ~ "West",
                           .$region %in% c("Eastern Asia","South-Eastern Asia") ~ "East Asia",
                           .$region %in% c("Caribbean","Central America","South America") ~ "Latin America",
                           .$continent=="Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
                           TRUE ~ "Others"
  ))
gapminder <- gapminder %>% mutate(group = factor(group, levels = c("Others","Latin America","East Asia","Sub-Saharan Africa","West")))
gapminder %>% filter(year %in% c(past_year,latest_year) & !is.na(gdp) & country %in% country_list) %>%
  group_by(year) %>% mutate(weight=population/sum(population)*2) %>% ungroup() %>% #weight countries based on population
  ggplot(aes(dollars_per_day, y=..count.., fill=group, weight=weight)) +  #differentiate between sizes of group using ..count.. variable found in geom_density function
  geom_density(alpha=.2,bw=.75,position="stack") +
  scale_x_continuous(trans="log2") +
  facet_grid(year ~ .)  
#refine groupings and look at infant survival rate
gapminder <- gapminder %>% 
  mutate(group = case_when(.$region %in% west ~ "The West",
                           .$region == "Northern Africa" ~ "Northern Africa",
                           .$region %in% c("Eastern Asia","South-Eastern Asia") ~ "East Asia",
                           .$region == "Southern Asia" ~ "Southern Asia",
                           .$region %in% c("Caribbean","Central America","South America") ~ "Latin America",
                           .$continent=="Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
                           .$region %in% c("Melanesia", "Micronesia","Polynesia") ~ "Pacific Islands")
  )
surv_income <- gapminder %>%
  filter(year %in% latest_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,infant_survival_rate = 1-sum(infant_mortality/1000*population)/sum(population))
surv_income %>% ggplot(aes(income,infant_survival_rate,label=group,color=group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981), breaks = c(.85,.90,.95,.99,.995,.998)) +
  geom_label(size=3, show.legend = FALSE)
#look at same thing but by country and notice the variability of country within a group
surv_income <- gapminder %>%
  filter(year %in% latest_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  mutate(income = gdp/population/365,infant_survival_rate = 1-infant_mortality/1000*population/population)
surv_income %>% ggplot(aes(income,infant_survival_rate,label=group,color=group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981), breaks = c(.85,.90,.95,.99,.995,.998)) +
  geom_point()

###############################
# explore fertility and infant mortality in Africa in 2012 by region
gapminder %>% filter(continent=="Africa" & year==2012) %>%
  ggplot(aes(fertility,life_expectancy,label=region,color=region)) +
  geom_point()
df <- filter(gapminder, continent=="Africa" & year==2012 & fertility<=3 & life_expectancy>=70) %>%
  select(country,region,fertility,life_expectancy)
# did the Vietnam war affect life expectancy
tab <- gapminder %>% filter(country %in% c("United States","Vietnam") & year>=1960 & year<=2010)
p <- tab %>% ggplot(aes(year,life_expectancy,color=country))
p + geom_line()
# what affect did the Vietnam war and dictatorship of Pol Pot have on Cambodia
gapminder %>% filter(country=="Cambodia" & year>=1960 & year<=2010) %>% ggplot(aes(year,life_expectancy)) + geom_line()
#dollars per day analysis of African countries
daydollars <- gapminder %>% filter(continent=="Africa" & year %in% c(1970,2010) & !is.na(gdp)) %>%
  mutate(dollars_per_day = gdp/population/365)
daydollars %>% ggplot(aes(dollars_per_day, fill=region)) +
  geom_density(bw=.5,position="stack") +
  scale_x_continuous(trans = "log2") + 
  facet_grid(year ~ .)
#infant mortality rate correlation with wealth
gapminder_Africa_2010 <- gapminder %>% filter(continent=="Africa" & year %in% c(1970,2010) & !is.na(gdp) & !is.na(infant_mortality)) %>%
  mutate(dollars_per_day = gdp/population/365)
gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day,infant_mortality,label=country,color=region)) +
  scale_x_continuous(trans = "log2") +
  geom_text(label=gapminder_Africa_2010$country,size=3) +
  facet_grid(year ~ .)

#######################################################################################
# Data Visualization Principles
#######################################################################################
library(dplyr)
library(ggplot2)
library(dslabs)
data(us_contagious_diseases)

#show measles rate in descending order using reorder function
dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting, state = reorder(state,rate))
dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()

#show box plot of regions murder rates showing all data so variability is understood
data("murders")
murders %>% mutate(rate = total/population*100000, region = reorder(region,rate)) %>%
  ggplot(aes(region,rate)) +
  geom_boxplot() +
  geom_point()

#create a slope chart for life expectancy vs fertility rates for a short timeframe
data("gapminder")
west <- c("Western Europe","Northern Europe","Southern Europe","Northern America","Australia and New Zealand")
gapminder %>% filter(year %in% c(2010,2015) & region %in% west & !is.na(life_expectancy) & population > 10^7) %>%
  mutate(location = ifelse(year==2010,1,2),
         location = ifelse(year==2015 & country %in% c("United Kingdom","Portugal"), location + .22, location),
         hjust = ifelse(year==2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year,life_expectancy,group=country)) +
  geom_line(aes(color=country), show.legend=FALSE) +
  geom_text(aes(x=location, label=country, hjust=hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy")

#look at infectious diseases and vaccines
#measles for example (not including Hawaii and Alaska because they became states in 1950s)
data("us_contagious_diseases")
dat <- us_contagious_diseases %>% filter(!state %in% c("Hawaii","Alaska") & disease=="Measles") %>%
  mutate(rate=count/population*10000, state=reorder(state,rate))
#plot California over time, measles vaccine was introduced in 1963
dat %>% filter(state=="California") %>%
  ggplot(aes(year,rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept = 1963, col="blue")
#show measle cases by state over time, vaccine introduced in 1963
library(RColorBrewer)
dat %>% ggplot(aes(year,state,fill=rate)) +
  geom_tile(color="grey50") + #use a tile color to represent measle rate
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors=brewer.pal(9,"Reds"),trans="sqrt") + #gradient color scale for tiles
  geom_vline(xintercept = 1963, col="blue") +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  ggtitle("Measles") +
  ylab("") +
  xlab("")
#really cool line chart that plots all of the state data with the average a bolded line through it
avg <- us_contagious_diseases %>% filter(disease=="Measles") %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)
dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(aes(year, us_rate), avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")

#functions to round with
x <- 123.348838492
signif(x, digits = 5)
round(x, digits = 3)

us_contagious_diseases %>% filter(!is.na(population)) %>% group_by(year,disease) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000) %>%
  ggplot(aes(year,us_rate,color=disease)) +
  geom_line()
