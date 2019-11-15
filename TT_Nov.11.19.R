library(tidyverse)
library(glue)
library(gganimate)
library(gifski)
library(png)

cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# Either ISO-8601 date or year/week works!
# Install via devtools::install_github("thebioengineer/tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load("2019-11-12")
tuesdata <- tidytuesdayR::tt_load(2019, week = 46)

cran_code <- tuesdata$loc_cran_packages

#Trying out code from Phillipe's article.  
languages <- cran_code %>% 
  count(language, sort = TRUE, name = "n_package")

#Still working through the article code.  Figuring out why things work is SUPER helpful!
#Bar chart displaying number of packages by language type
languages %>% 
  top_n(16, n_package) %>% 
  mutate(language = fct_reorder(language, n_package)) %>% 
  ggplot(aes(x = language, y = n_package)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.2))) +
  xlab(NULL) +
  ylab("Number of package") +
  labs(title = str_wrap("Number of R packages using the top 16 most used programming languages", 40)) +
  labs(subtitle = glue("Based on {n_distinct(cran_code$pkg_name)} packages")) +
  labs(caption = "Data: https://cran.r-project.org/src/contrib/")

#You guessed it.  Still working through the article code. 
most_popular <- cran_code %>% 
  group_by(language) %>% 
  summarise(total_loc = sum(code)) %>% 
  filter(dense_rank(desc(total_loc)) <= 16) %>% 
  mutate(language = fct_reorder(language, total_loc, .fun = sum))

#Similar to the previous bar chart.  This one shows the 16 most popular languages and their associated lines of code
most_popular %>%
  ggplot(aes(x = language, y = total_loc)) +
  geom_col() +
  coord_flip() +
  xlab(NULL) +
  ylab("Number of line of code") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.2)), labels = scales::comma) +
  labs(
    title = str_wrap("Top 16 programming languages used in R packages", 40),
    subtitle = glue("Based on {n_distinct(cran_code$pkg_name)} packages"),
    caption = "Data: https://cran.r-project.org/src/contrib/"
  )

#I want to pull the tidyverse packages and then animate through them showing different compositions of code.
Tidy_Code <- cran_code %>%
  filter( pkg_name %in% c(
    "ggplot2",
    "dplyr",
    "tidyr",
    "readr",
    "purrr",
    "tibble",
    "stringr",
    "focats",
    "lubridate",
    "hms", 
    "feather", 
    "haven",
    "jsonlite",
    "readxl",
    "rvest",
    "xml2",
    "modelr",
    "broom"))

#Here's the static bar chart
p <- ggplot(Tidy_Code, aes(x=language, y=code, group=pkg_name, color=language, fill=language))+
  geom_col(show.legend=F)+
  coord_flip()+
  labs( x="Language", y="Lines of Code", title="Types of Language in the Tidyverse")+
  theme_bw()

#Here's the animation
anim<- p + transition_states(pkg_name, transition_length = 500, state_length = 500)+
  enter_grow()+
  exit_shrink()+
  ease_aes('sine-in-out')+
  labs(subtitle="Tidyverse Package: {closest_state} ")

anim_save("Tidyverse_Languages.gif", anim)

