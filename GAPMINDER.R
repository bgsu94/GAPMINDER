ls()

install.packages("learningr")
install.packages("gapminder")
install.packages("rmarkdown")
install.packages("tinytex")
install.packages("ISLR2")

library(readxl)
library(tidyverse)
library(janitor)
library(palmerpenguins)
library(learningr)
library(gapminder)
library(ggplot2)
library(rmarkdown)
library(tinytex)
library(dplyr)
library(ISLR2)
library("data.table")

#reshape from long 2 wide
data(gapminder)
View (gapminder)

gapminder_data <- select(gapminder, country, year, lifeExp)
View(gapminder_data)

gapminder_pop_data <- select(gapminder, country, year, pop)
View(gapminder_pop_data)

gapminder_wide_data <- gapminder_data %>%
  pivot_wider(names_from = year, values_from = lifeExp)
View(gapminder_wide_data) 

gapminder_long_data <- gapminder_wide_data %>%
  pivot_longer(2:13,
               names_to = "YEAR",
               values_to = "LIFEEXP")
View(gapminder_long_data)

gapminder %>%
  filter(continent %in% c("Africa", "Europe")) %>%
  t.test(lifeExp ~ continent, data = .,
         altrnative = "two.sided",
         paired = FALSE)

gapminder_pop_data %>%
  filter(country == "Costa Rica" |
           country == "Sri Lanka" |
           country == "Uganda") %>%
  ggplot(aes(year, pop, colour = country))+
  geom_point(size =5, alpha = 0.3)+
  geom_line(size = 1)+
  theme_minimal()+
  labs(title = "Best Flags Population Timeline")
ggsave("Best Flags Population Timeline.pdf")