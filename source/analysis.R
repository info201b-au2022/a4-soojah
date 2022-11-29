library(tidyverse)
library("ggplot2")
# The functions might be useful for A4
source("../source/a4-helpers.R")

incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# population of white people in jail in year 2002 (my birth year)
pop_white_jail <- incarceration %>%
  filter(year == 2002) %>%
  summarise(white_total_jail = sum(pop_white_jail, na.rm = TRUE))

# population of black people in jail in year 2002 (my birth year)
pop_black_jail <- incarceration %>%
  filter(year == 2002) %>%
  summarise(black_total_jail = sum(pop_black_jail, na.rm = TRUE))

# total population of white people in 2002 (my birth year)
total_white_pop_2002 <- incarceration %>%
  filter(year == 2002) %>%
  summarise(white_total = sum(total_white_pop_2002, na.rm = TRUE))

# total population of black people 2002 (my birth year)
total_black_pop_2002 <- incarceration %>%
  filter(year == 2002) %>%
  summarise(black_total = sum(total_black_pop_2002, na.rm = TRUE))

# calculate ratio of white population to white population in jail
white_jail_ratio <- pop_white_jail$white_total_jail / total_white_pop_2002$white_total
# calculate ratio of black population to black population in jail
black_jail_ratio <- pop_black_jail$black_total_jail / total_black_pop_2002$black_total

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function wrangles data jail population in U.S.
get_year_jail_pop <- function() {
  # Call initial data
  total_pop <- incarceration %>%
    # Pipe group by year 
    group_by(year) %>%
    # Pipe and summarise by year 
    summarise(total_jail_population = sum(total_jail_pop, na.rm = TRUE))
  return(total_pop)   
}


# This function plots jail population in U.S.
plot_jail_pop_for_us <- function()  {
  pop_graph <- ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total_jail_population)) +
    labs(title = "Growth of US Prison Population", caption = "Shows growth of US prison population levels between 1970 and 2018") +
    scale_y_continuous(labels = scales::comma) 
  return(pop_graph)   
} 


plot_jail_pop_for_us()

## Section 4  ---- 

#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# ---------------------------------------------------------------------------#
# create vector of chosen states
states <- c("AL", "CA", "OR", "WA", "TX")

# wrangle data jail population by states
get_jail_pop_by_states <- function(states) {
  state_total <- incarceration %>%
    # pipe from incarceration to filter states
    filter(state %in% states) %>%
    # group by year and state
    group_by(year, state) %>%
    # summarise total jail population by grouped year and state 
    summarise(total = sum(total_jail_pop, na.rm = TRUE))
  return(state_total)
}

plot_jail_pop_by_states <- function(states) {
  # create a variable to store plot from previous
  jail_plot <- ggplot(get_jail_pop_by_states(states)) +
    # create line graph 
    geom_line(
      # use aesthetic to plot line graph by setting x axis to year
      mapping = aes(x = year,
                    # set y axis to total_jail_state_population
                    y= Total_Jail_State_Population,
                    # distinguish different state line graph by color 
                    color = state)
    ) +
    # create labels for the graph 
    labs(
      title = "Growth of Prison Population by State",
      caption = "This chart illustrates the growth in prison populations of various states"
    )+
    # scale numbers
    scale_y_continuous(labels = scales::comma)
  return(line_pop)
}


plot_jail_pop_by_states()

## Section 5  ---- 
jail_gender_pops <- function() {
  # create data frame with male and female jail populations using pipe
  gender_data_frame <- data.frame(female_jail_poulation = incarceration_data$female_jail_pop,
                   male_jail_pop = incarceration_data$male_jail_pop)
  return(gender_data_frame)
}

plot_jail_gender_pop <- function() {
  # initialize variable for plot
  scatter_plot <- ggplot(jail_gender_pops(),  
                        # aesthetic plots with x female and y male by year
                        aes(x = female_jail_pop,y = male_jail_pop, group = year)) +
    geom_point(aes(color = year))+
    # add labels to the graph 
    labs(
      title = "Comparison of Male and Female Jail Populations",
      caption = "portrays comparison of male and female population from 1970-2010"
    ) +
    # scale into numbers
    scale_y_continuous(labels = scales::comma)
  return(scatter_plot)
}

plot_gender_jail_pop()
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 

get_female_state_jail_pop <- function() {
  # lowercase state collumn
  incarceration$state <- tolower(state.name[match(incarceration$state, state.abb)])
  
  # group bt state after pipe
  df<- incarceration %>%
    group_by(state) %>%
    summarise(sum_female_pop = sum(female_jail_pop, na.rm = TRUE))
  df_na <- df %>% drop_na(state)
  return(df_na)
}

# View(get_female_state_jail_pop())

plot_female_state_jail_pop <- function(){
  # create map and rename for join
  state_map <- map_data("state") %>%
    rename(state = region) %>%
    left_join(get_female_state_jail_pop(), by="state")
  
  # draw of each female prison population 
  map_plot <- ggplot(state_map) +
    geom_polygon(
      # set x axis to longitude and y axis to latitude to draw the map 
      mapping = aes(x = long, y = lat, group = group, fill = sum_female_pop), 
      # seperate with white lines
      color = "white", size = .1)+ 
    coord_map() +
    # decide a color for the high and low spectrum of female prison population in each state
    scale_fill_continuous(labels = scales :: comma, low = "Red", high = "Blue") +
    # add labels to the graph 
    labs(
      title = "Female jail population by state",
      caption = "illustrates female prison populations by state"
    ) +
    # clear axis
    xlab(" ") +
    ylab(" ") + 
    # scale numbers
    scale_y_continuous(labels = scales::comma) 
  return(map_plot)
}

plot_female_state_jail_pop()

get_male_state_jail_pop <- function() {
  # make the state column into lowercase and full name otherwise it won't match up with the map
  incarceration$state <- tolower(state.name[match(incarceration$state, state.abb)])
  
  male_state_pop <- incarceration %>%
    # group by state that is piped from incarceration
    group_by(state) %>%
    # summarise the sum of male_jail_pop 
    summarise(sum_male_pop = sum(male_jail_pop, na.rm = TRUE))
  # get rid of any NA values in state column 
  male_state_pop_without_na <- df %>% drop_na(state)
  return(male_state_pop_without_na)
}

# View(get_male_state_jail_pop())

plot_male_state_jail_pop <- function(){
  # create map and join
  state_shape <- map_data("state") %>%
    rename(state = region) %>%
    left_join(get_male_state_jail_pop(), by="state")
  
  # draw the map by setting fill of each male prison population 
  map_plot <- ggplot(state_shape) +
    # plot shapes and draw map as x longitude y latitude
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = sum_male_pop), 
      # seperate with line
      color = "white", 
      size = .1)+ 
    coord_map() +
    # set high and low color scales
    scale_fill_continuous(labels = scales :: comma, low = "Red", high = "Blue") +
    # add labels to the graph 
    labs(
      title = "Male jail population by state",
      caption = "illustrates male prison populations by state"
    ) +
    # clear axis
    xlab(" ") +
    ylab(" ") + 
    # scale numbers
    scale_y_continuous(labels = scales::comma) 
  return(map_plot)
}

plot_male_state_jail_pop()


#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


