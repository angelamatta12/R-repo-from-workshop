library(tidyverse)
install.packages("tidyverse")

gapminder_1997 <- read_csv("gapminder_1997.csv")

name <- "Angela"
Sys.Date()
gapminder_1997

round(3.1415,2)

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

gapminder_data <- read_csv("gapminder_data.csv")

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

ggsave("figures/Rplot.png")
