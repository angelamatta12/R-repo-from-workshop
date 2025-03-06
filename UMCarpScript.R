library(tidyverse)
install.packages("tidyverse")

gapminder_1997 <- read_csv("data/gapminder_1997.csv")
gapminder_data <- read_csv("data/gapminder_data.csv")

# --- summarize the stats --- #

#summarize(gapminder_data, avgLifeExp = mean(lifeExp))
# --- same thing, diff notation --- # 
gapminder_data_summarize <- gapminder_data %>% summarize(avgLifeExp = mean(lifeExp), nrows = n())

print(gapminder_data_summarize)

gapminder_data %>% summarize(avgPop = mean(pop))

gapminder_data %>% summarize(recentYr = max(year))

gapminder_avgLife_2007 <- gapminder_data %>% filter(year == 2007 ) %>% summarize(avgLifeExp = mean(lifeExp))

year_of_interest <- gapminder_data %>% summarize(recentYr = min(year)) 

year_of_interest$recentYr 

gapminder_data %>% filter(year == year_of_interest$recentYr 
 ) %>% summarize(avgGDP = mean(gdpPercap))


gapminder_data %>% group_by(year)%>% summarize(avgLifeExp = mean(lifeExp)) 

gapminder_data %>% group_by(continent)%>% summarize(avgLifeExp = mean(lifeExp)) 

gapminder_data %>% mutate(pop *gdpPercap)

gapminder_data %>% mutate(popInMil = pop/1e6)

# subsets using select, so some SQL related stuff in R 

gapminder_data %>% select(pop,year)

gapminder_data %>% select(-year) # gets rid of only year column 


gapminder_data %>% select(-pop, -gdpPercap)

gapminder_data %>% select(starts_with("c")) # column name starts with c 
gapminder_data %>% select(ends_with("p")) # column name ends with p 

unique(gapminder_data$year) # get unique years 

# change data shape long format to wide format

gapminder_data %>% select(-pop, -gdpPercap) %>% pivot_wider(names_from = year, values_from = lifeExp)

# final data, read in gapminder, filter out 2007, americas, dont include year and continent columns

gapminder_data %>% filter(year == 2007 & continent == 'Americas') %>% select(-year, -continent)

# --- exploring the types of graphs that we can do --- #

name <- "Angela"
ggplot(data = gapminder_1997) + 
  aes(x=gdpPercap) +
  labs(x = "GDP per Capita") + 
  aes(y=lifeExp) +
  labs(y = "Life Expectancy") + 
  geom_point() +
  labs(title = "Do wealthy people live longer? ") +
  aes(color = continent) +
  scale_color_manual(values = wes_palette("Rushmore1")) +
  aes(size = pop/1000000) +
  labs(size = "population in millions")



dim(gapminder_1997)
head(gapminder_1997)
ggplot(gapminder_1997) +
  aes(x=continent) +
  labs(x = "continent") + 
  aes(y=lifeExp) +
  labs(y = "Life Expectancy") + 
  geom_boxplot() +
  labs(title = "Life Expectancy per continent") +
  aes(color = continent)



dim(gapminder_1997)
head(gapminder_1997)
ggplot(gapminder_1997) +
  aes(x=continent) +
  labs(x = "continent") + 
  aes(y=lifeExp) +
  labs(y = "Life Expectancy") + 
  geom_violin() +
  geom_jitter(aes(size = pop))+ 
  labs(title = "Life Expectancy per continent") +
  aes(color = continent)

ggplot(gapminder_1997) +
  aes(x=lifeExp) +
  labs(x = "Life Expectancy") + 
  geom_histogram(bins = 50 ) +
  labs(title = "Life Expectancy") +
  aes(color = continent)

ggplot(gapminder_1997) +
  aes(x= gdpPercap,y = lifeExp) +
  labs(x = "Life Expectancy") + 
  geom_point() +
  facet_wrap(vars(continent)) +
  labs(title = "Life Expectancy") +
  aes(color = continent)

ggplot(gapminder_1997) +
  aes(x= gdpPercap,y = lifeExp) +
  labs(x = "Life Expectancy") + 
  geom_point() +
  facet_grid(rows = vars(continent)) +
  labs(title = "Life Expectancy") +
  aes(color = continent)

ggsave("Rplot.png")