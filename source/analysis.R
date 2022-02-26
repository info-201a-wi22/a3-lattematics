library(ggplot2)
library(dplyr)
library(usmap)

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# chart 1 - gender population across time 
gender <- incarceration_trends %>%
  select(female_jail_pop, male_jail_pop, year, total_pop)

female_avg <- gender %>%
  group_by(year) %>%
  summarise(female_jail_pop = mean(female_jail_pop, na.rm = TRUE))

male_avg <- gender %>%
  group_by(year) %>%
  summarise(male_jail_pop = mean(male_jail_pop, na.rm = TRUE))

gender_vs_time <- ggplot(data = NULL) +
  geom_line(data = female_avg, 
            aes(x = year, y = female_jail_pop, color = "purple")) + 
  geom_line(data = male_avg, 
            aes(x = year, y = male_jail_pop, color = "yellow")) + 
  ggtitle("Incarceration by Gender") +
  labs(x = "Years", y = "Jail Population") +
  scale_color_discrete(name = "Gender", 
                       label = c("Female", "Male"))

# chart 2 - variable comparison, gender and race
race <- incarceration_trends %>%
  select(black_female_prison_pop, 
         black_male_prison_pop, 
         white_female_prison_pop, 
         white_male_prison_pop)

race <- na.omit(race)

female_vs_male <- ggplot(data = race) +
  geom_line(aes(x = black_female_prison_pop, y = black_male_prison_pop, color = "orange")) + 
  geom_line(aes(x = white_female_prison_pop, y = white_male_prison_pop, color = "purple")) +
  ggtitle("Incarceration by Race and Gender") +
  labs(x = "Female", y = "Male") +
  scale_color_discrete(name = "Race", 
                       label = c("Black", "White"))

highest_black_female_pop <- race %>%
  filter(black_female_prison_pop == max(black_female_prison_pop)) %>%
  pull(black_female_prison_pop)

highest_black_male_pop <- race %>%
  filter(black_male_prison_pop == max(black_male_prison_pop)) %>%
  pull(black_male_prison_pop) 

highest_white_female_pop <- race %>%
  filter(white_female_prison_pop == max(white_female_prison_pop)) %>%
  pull(white_female_prison_pop) 

highest_white_male_pop <- race %>%
  filter(white_male_prison_pop == max(white_male_prison_pop)) %>%
  pull(white_male_prison_pop) 

ratio_female <- highest_black_female_pop / highest_white_female_pop
ratio_male <- highest_black_male_pop / highest_white_male_pop

# map - gender ratio, male/female
gender_state <- incarceration_trends %>%
  select(female_jail_pop, male_jail_pop, year, total_pop, state) %>%
  mutate(female_num = female_jail_pop * total_pop) %>%
  mutate(male_num = male_jail_pop *total_pop) %>%
  group_by(state) 

gender_state <- na.omit(gender_state)

gender_state <- gender_state %>%
  mutate(male_to_female = male_num / female_num) %>%
  filter_all(all_vars(!is.infinite(male_to_female)))

plot_usmap(data = gender_state, values = "male_to_female") +
  scale_fill_continuous(name = "Gender ratio (male/female)")
