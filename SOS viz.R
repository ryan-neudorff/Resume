library(tidyverse)
library(gtExtras)
library(gt)
library(cfbfastR)
library(crop)
library(nflreadr)
library(magick)
library(glue)

SP = cfbd_ratings_sp(year = 2025)
conf = SP %>% filter(conference == 'Big Ten') %>% select(team)
SP = SP %>% mutate(opponent = team) %>% select(opponent, ranking,offense_ranking,defense_ranking)

plot_data <- espn_cfb_schedule(year = 2025)%>% 
  filter(is.na(home_record)) %>%
  select(home_team = home_team_location, away_team = away_team_location, date = game_date) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  filter(home_team %in% conf$team | away_team %in% conf$team) %>%
  select(home_team, away_team) %>% 
  nflreadr::clean_homeaway() %>% 
  filter(team %in% conf$team & opponent %in% conf$team)

dat = plot_data %>% left_join(.,SP,by = "opponent") %>% 
  group_by(team) %>% summarise(SOS = round(mean(ranking, na.rm = T),2), SOS_O = round(mean(offense_ranking, na.rm = T),2),
                               SOS_D = round(mean(defense_ranking, na.rm = T),2))
  
team = cfbd_team_info(year = 2025) %>% select(school,logo) %>% rename(team = school)

data = dat %>% left_join(.,team, by = "team") %>% arrange(SOS) %>%
  mutate(Rk = row_number()) %>% select(6,5,2) %>%
  mutate(group = rep(1:3, each = 6)) %>% group_split(group) %>% bind_cols()

## Table time

gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font('Franklin Gothic ATF Medium') %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_style(
      style = cell_text(font = 'BC Sklonar Light'),
      locations = cells_body(columns = c(3,7,11))
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}

title_header = glue(
  "<div style='display: flex; justify-content: space-between; align-items: center;'>
    <div>
        <img src='https://upload.wikimedia.org/wikipedia/commons/1/1b/Big_Ten_Conference_logo.png' style='height: 50px; width: auto; vertical-align: left;'>
    </div>
    <div style='text-align: center;padding-left: 50px;'>
        <span style='font-weight: bold; font-size: 30px; line-height: 1.2;'>2025 Road to Indy </span> <br> 
        <span style= 'font-weight: normal; font-size: 15px;line-height: 1.2;'>Which teams have the most difficult path? </span>
    </div>
  </div>"
)


gt(data) %>%
  tab_header(title = html(title_header)) %>%
  gt_theme_538() %>%
  gt_img_multi_rows(columns = 2, img_source = 'web', height = 30) %>%
  gt_img_multi_rows(columns = 6, img_source = 'web', height = 30) %>%
  gt_img_multi_rows(columns = 10, img_source = 'web', height = 30) %>%
  cols_align(columns = everything(), 'center') %>%
  cols_hide(c(4,8,12)) %>%
  cols_label(
    contains('logo') ~ "",
    contains('Rk') ~ 'Rk',
    contains('SOS') ~ 'SOS'
  ) %>%
  data_color(
    columns = c(3,7,11),
    palette = c('#8bd2f7','#ffffff','#f5fc8d'),
    domain = c(26,52)) %>%
  tab_source_note(md('Data by cfbfastR | Viz by @ryan_neudorff')) %>%
  tab_style(
    style = cell_text(color = '#bcbab6', font = 'Franklin Gothic ATF'),
    locations = cells_source_notes()
  ) %>%
  gtsave(filename = 'B1G SOS.png', path = 'C:/Users/ryann/OneDrive/Documents/R Projects/Graphic', zoom= 2,expand = 5)

