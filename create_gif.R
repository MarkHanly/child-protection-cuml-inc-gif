#########################################################################################################
# Project:  Incidence of child maltreatment
# Purpose:  Illustrate cumulative incidence of child maltreatment between birth and age five as a gif
# Inputs:   20200624_dailycumlhazards.xlsx
# Outputs:  figure1.gif, figure2.gif
#########################################################################################################


# load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(gganimate)

# Read in data
data <- readxl::read_excel("20200624_dailycumlhazards.xlsx", col_names = TRUE)

# Define the different levels 
levels1 <- c("rosh" = "Notifications", 
             "inv" = "Investigations", 
             "sub" = "Substantiations",
             "oohc" = "OOHC")

breaks1 <- c("rosh", "inv", "sub", "oohc")

levels2 <- c("Emotional abuse",
             "Neglect",
             "Physical abuse",
             "Sexual abuse")

breaks2 <- c("emotional", "neglect", "physical", "sexual")

# Create the plot - main outcomes
p1 <- data %>%
  filter(time%%7 == 1) %>%
  filter(var %in% breaks1) %>% 
  mutate(years = time/365.4,
         varlab = recode(var, "rosh" = "Notifications", 
                         "inv" = "Investigations", 
                         "sub" = "Substantiations",
                         "oohc" = "OOHC")) %>% 
  ggplot(aes(x=years, y=cumhaz, group=var)) +
    geom_line(aes(color = var), size=2) +
    geom_label(aes(x = years, y=cumhaz, group=var, label = paste0(varlab, ": ", format(round(100*cumhaz, digits=1), nsmall=1), "%")), 
               hjust = 0.8, vjust=-.15) +
    coord_cartesian(clip = 'off') +
    scale_y_continuous(labels = scales::percent, limits = c(0,.15)) +
    labs(x = "Age (years)", y = "Population (%)", color = "CPS Involvement", 
         title = "How many children are involved with child protection services\nbefore their fifth birthday?" ,
         subtitle = "{format(plyr::round_any(frame_along, .5), nsmall=1)} Years",
         caption = "Data from Falster et al (2020) JAMA Pediatrics \n doi:10.1001/jamapediatrics.2020.1151") +
    scale_colour_discrete(labels = levels1, breaks = breaks1) +
    theme_minimal(base_size = 20) +
    transition_reveal(years)

# Animate the plot
animate(p1, renderer = gifski_renderer(loop = FALSE), nframes = 100, fps = 1000, end_pause = 10, height=500, width = 800)

# Save the animations
anim_save("figure1.gif")

# Create the plot - tyoes of substantiations
p2 <- data %>%
  filter(time%%7 == 1) %>%
  filter(var %in% breaks2) %>% 
  mutate(years = time/365.4,
         varlab = recode(var, "emotional" = "Emotional abuse", 
                         "neglect" = "Neglect", 
                         "physical" = "Physical abuse",
                         "sexual" = "Sexual abuse")) %>% 
  ggplot(aes(x=years, y=cumhaz, group=var)) +
  geom_line(aes(color = var), size=2) +
  geom_label(aes(x = years, y=cumhaz, group=var, label = paste0(varlab, ": ", format(round(100*cumhaz, digits=1), nsmall=1), "%")), 
             hjust = 0.8, vjust=-.15) +
  coord_cartesian(clip = 'off') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,.02)) +
  labs(x = "Age (years)", y = "Population (%)", color = "Maltreatment type", 
       title = "Cumulative Incidence of the Primary Substantiated \n Maltreatment Type From Ages 0 to 5 Years",
       subtitle = "{format(plyr::round_any(frame_along, .5), nsmall=1)} Years",
       caption = "Data from Falster et al (2020) JAMA Pediatrics \n doi:10.1001/jamapediatrics.2020.1151") +
  scale_colour_discrete(labels = levels2, breaks = breaks2) +
  theme_minimal(base_size = 20) +
  transition_reveal(years)

# Animate the plot
animate(p2, renderer = gifski_renderer(loop = FALSE), nframes = 100, fps = 10, end_pause = 10, height=500, width = 800)

# Save the animations
anim_save("figure2.gif")

