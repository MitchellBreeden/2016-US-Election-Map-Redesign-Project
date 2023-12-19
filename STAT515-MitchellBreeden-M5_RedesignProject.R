#Load libraries
library(stringr)
library(dplyr)
library(tilegramsR)
library(ggplot2)
library(ggtext)
library(ggthemes)

#Read csv and create lists
df = read.csv("States.csv")
states_names = as.list(df$State)
republican = as.list(df$Republican)
democrat = as.list(df$Democrat)

#Append full state names
sf_FiveThirtyEightElectoralCollege.states <- sf_FiveThirtyEightElectoralCollege.states %>%
  mutate(states_names)

#Create congressional districts
sf_maine_districts <- sf_FiveThirtyEightElectoralCollege %>%
  filter(state == "ME") %>%
  mutate(district = c("ME", "ME", "ME1", "ME2"))

#Append remaining states
sf_final <- sf_FiveThirtyEightElectoralCollege %>%
  mutate(district = state) %>%
  filter(state != "ME") %>%
  bind_rows(sf_maine_districts)

#Assign winner/color to party
sf_results <- sf_final %>%
  mutate(winner = ifelse(district %in% democrat, "Democrat", "Republican"),
         color = ifelse(winner == "Democrat", "#1405BD", "#DE0100"))

#Plot
plot_country <- ggplot() +
  geom_sf(
    data = sf_results,
    fill = sf_results$color,
    color = "#d0d0d0",
    size = .09
  ) +
  geom_sf(
    data = sf_FiveThirtyEightElectoralCollege.states,
    color = "white",
    alpha = 0,
    size = .25
  ) +
  geom_sf_text(
    data = sf_FiveThirtyEightElectoralCollege.states,
    aes(label = state),
    color = "white",
    size = 3
  ) +
  theme_map()

#Add title
plot_country <- plot_country +
  labs(
    title = "<span style='font-size:50pt'>
    2016 US Presidential Election</span>",
    subtitle = "<span style='font-size:20pt'>Electoral college votes for
    <span style='color:#1405BD;'>Hillary Clinton</span> (232) and 
    <span style='color:#DE0100;'>Donald Trump</span> (306)
    </span>"
  ) +
  theme(
    plot.title = element_textbox(hjust = .5),
    plot.subtitle = element_textbox(hjust = .5)
  )

#Create state enlarger function
plot_state <- function(state_initial) {
  
  #Filter for specified state
  sf_state <- sf_results %>% 
    filter(state == state_initial)
  
  #Filter for state name
  sf_state_full <- sf_FiveThirtyEightElectoralCollege.states %>%
    filter(state == state_initial)
  
  #Plot state
  plot_state <- ggplot() +
    geom_sf(
      data = sf_state,
      fill = sf_state$color, 
      color = "#d0d0d0"
      ) +
    theme_map()
  
  #Add title
  plot_state <- plot_state +
  labs(
    title = "<span style='font-size:40pt'>
    2016 US Presidential Election</span>",
    subtitle = paste(sf_state_full$states_names, "-", sf_state$tilegramVa[1], "Electoral Votes")
  ) +
    theme(
      plot.title = element_textbox(hjust = .5),
      plot.subtitle = element_textbox(hjust = .5, size = 25)
    )
  
  return(plot_state)
  
}

#Show whole country
plot_country

#Show specified state
plot_state("VA")