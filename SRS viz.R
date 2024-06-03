#Creating a visualization for our Simple Rating System (SRS) data set
#load in the required packages

library(readr)
library(tidyverse)
library(gtExtras)
library(gt)
library(cfbfastR)

#load in our data set that we created in Python
rankings <- read_csv("C:/Users/ryann/Downloads/SRS_rankings.csv")
#Adding a Rank column
rankings$rank <- 1:nrow(rankings)

#Loading in team logos
team <- cfbfastR::cfbd_team_info(year = 2023) %>% dplyr::select(school,logo)
team <- team %>% rename(team = school)

#Joining both the Ranking an team data sets into one.
rankings <- rankings %>% left_join(.,team, by = "team")

#Creating a Second Data set that only shows the top 25
rank <- rankings %>% slice(1:25) %>%select(rank, logo, team, rating)
rankings <- rankings%>%select(rank, logo, team, rating)

#Creating the top 25 graphic
g1 <- rank %>% gt() %>% gt_theme_538() %>% tab_header(title = md("**SRS Top 25**"), subtitle = 'Rankings as of Jan 8, 2024') %>%
  gt::data_color(columns = rating, colors = c("darkred","white","darkgreen")) %>% cols_align(align = "center",columns = c(rank,rating)) %>%
  tab_style(style = cell_text(weight = 'bold'), locations = cells_body(columns = c(rank,team,rating))) %>%
  tab_style(style = cell_text(weight = 'bold'), locations = cells_column_labels()) %>%
  fmt_number(columns = rating, decimals = 2) %>% gt_img_rows(columns = logo, img_source = "web", height = 25) %>%
  tab_style(style = cell_borders(sides = c("bottom"),color = "white",weight = px(3),style="solid"),
    locations = list(cells_body(columns = 1,rows = c(1:25)))) %>%
  cols_label(rank = "Rk", team = "Team", rating = "SRS", logo = "") %>%
  tab_source_note(md('Data by CFBD + cfbfastr <br> Viz. by Ryan Neudorff'))
g1

#Creating the full team graphic
g2 <- rankings %>% gt() %>% gt_theme_538() %>% tab_header(title = md("**SRS Full Team Rankings**"), subtitle = 'Rankings as of Jan 8, 2024') %>%
  gt::data_color(columns = rating, colors = c("darkred","white","darkgreen")) %>% cols_align(align = "center",columns = c(rank,rating)) %>%
  tab_style(style = cell_text(weight = 'bold'), locations = cells_body(columns = c(rank,team,rating))) %>%
  tab_style(style = cell_text(weight = 'bold'), locations = cells_column_labels()) %>%
  fmt_number(columns = rating, decimals = 2) %>% gt_img_rows(columns = logo, img_source = "web", height = 25) %>%
  tab_style(style = cell_borders(sides = c("bottom"),color = "white",weight = px(3),style="solid"),
            locations = list(cells_body(columns = 1,rows = c(1:133)))) %>%
  cols_label(rank = "Rk", team = "Team", rating = "SRS", logo = "") %>%
  tab_source_note(md('Data by CFBD + cfbfastr <br> Viz. by Ryan Neudorff'))
g2
