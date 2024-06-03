library(tidyverse)
library(gtExtras)
library(gt)
library(cfbfastR)
library(readxl)
library(nflreadr)
library(glue)

SP <- read_excel("SP+.xlsx")

schedule <- espn_cfb_schedule(year = 2024, limit = 1000) %>% 
  filter(is.na(home_record)) %>% # remove games that have occured
  select(home_team = home_team_location, away_team = away_team_location, date = game_date) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%dT%H:%MZ"))

weeks <- espn_cfb_calendar(year = 2024) %>% 
  select(week, start_date, end_date) %>% 
  mutate(across(-week, ~as.Date(.x, format = "%Y-%m-%dT%H:%MZ")))

schedule <- left_join(
  schedule,
  weeks,
  join_by(between(date, start_date, end_date))
)

b1g = c("Illinois", "Indiana", "Iowa", "Nebraska", "Northwestern", "Michigan", "Michigan State", "Minnesota",
        "Ohio State", "Penn State", "Rutgers", "Wisconsin", "Purdue", "Maryland", "Washington", "USC", "UCLA", "Oregon")

schedule <- schedule %>% filter(home_team %in% b1g | away_team %in% b1g)

plot_data <- schedule %>% 
  select(home_team, away_team) %>% 
  nflreadr::clean_homeaway() %>% 
  filter(team %in% b1g)

plot_data = plot_data %>% select(team, Team =opponent)
plot_data = merge(plot_data,SP,by = "Team")
plot_data = plot_data %>% group_by(team) %>% summarise(SOS = round(mean(Rk),2))

team <- cfbfastR::cfbd_team_info(year = 2023) %>% dplyr::select(school,logo)
team <- team %>% rename(team = school)

plot_data = plot_data %>% left_join(.,team, by = "team") 
plot_data = plot_data %>% select(logo,team,SOS) %>% arrange(SOS) %>% slice(1:10)


h1 = plot_data %>% gt() %>% tab_header(title = md('**The Road to Indy**'),subtitle = 'Which Teams have the Hardest Schedule in the Big Ten in 2024?') %>% gt_theme_538() %>% 
  gt_img_rows(columns = logo, img_source = "web", height = 30) %>%
  tab_footnote(footnote = md("*Top 10 Hardest Shown.*"),locations = cells_title(groups = "subtitle")) %>%
  tab_style(cell_borders(sides = "bottom",color = "lightgray",weight = px(1.5),style="dotted"),locations = cells_body(rows = everything())) %>%
  tab_style(cell_borders(sides = "left",color = "darkgrey",weight = px(.5)),locations = cells_body(columns = SOS)) %>%
  tab_style(cell_borders(sides = "right",color = "darkgrey",weight = px(.5)),locations = cells_body(columns = SOS)) %>%
  tab_style(style = cell_text(weight = 'bold'), locations = cells_column_labels()) %>%
  tab_style(style = cell_text(weight = 'bold'), locations = cells_body(columns = SOS)) %>%
  data_color(columns = SOS, colors = c('cadetblue1','white')) %>%
  tab_styl
  cols_align(align = "center",columns = SOS) %>%
  tab_source_note(md('Data Provided by @ESPN_BillC + cfbfastr<br>Viz. by @ryan_neudorff'))
h1
