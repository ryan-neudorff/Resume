#Load in the packages
library(tidyverse)
library(rvest)
library(readr)
library(openxlsx)
library(data.table)
library(gt)
library(gtable)
library(purrr)

date = as.Date(Sys.Date()-1)

ID = data.frame()
for(i in date){
  html = read_html(paste0('https://stats.ncaa.org/season_divisions/18403/livestream_scoreboards?utf8=%E2%9C%93&season_division_id=&game_date=',as.Date(i),'&conference_id=0&tournament_id=&commit=Submit'))
  html_analysis = html %>% html_elements('table') %>% html_elements('tr')
  id = html_analysis %>% html_element('table') %>% html_attr('id')
  ids = na.omit(data.frame(id = unlist(id)))
  ids = ids %>% separate_wider_delim(.,id,"_", names = c('not','ids','not2')) %>%
  select(ids)

ID = distinct(rbind(ID,ids))
write.csv(ID,'C:/Users/ryann/OneDrive/Documents/R Projects/game_ids.csv')
}

x = paste0('https://stats.ncaa.org/contests/',ID$ids,'/play_by_play')
url = data.frame(x)

master_pbp = data.frame()
for(j in url$x){
  html = read_html(j)
  html_analysis = html %>% html_table()
  pbp = rbindlist(html_analysis,fill = T)

pbp = pbp %>% select(ncol(.)-3,ncol(.)-2,ncol(.)-1,ncol(.)) %>% na.omit() %>% mutate(rowid_to_column(.,"ID")) %>%
  mutate(home = .[[4]], away = .[[2]]) %>% select(Time,away,Score,home,ID) %>%
  mutate(half = case_when(ID < which(home == 'period start')[2]~1,
                          ID >= which(home == 'period start')[2] & 
                            ID < which(home == 'period start')[3]~2,
                          ID >= which(home == 'period start')[3]~3,T~2)) %>%
  separate_wider_delim(.,cols=Time,delim = ':', names = c("min",'sec','mic')) %>%
  mutate_at(vars(min,sec,mic),as.numeric) %>%
  mutate(min = min*60, Time = min+sec,qtr = case_when(half == 1 & Time >= 600 ~ 1,
                                                      half == 1 & Time < 600 ~ 2,
                                                      half == 2 & Time >= 600 ~ 3,
                                                      half == 2 & Time < 600 ~ 4, T~5)) %>%
  select(half,qtr,away,home)

box = pbp %>% mutate(two_att = case_when(grepl('2pt',home)~1, T~0),
         md = case_when(grepl('made',home)~1,T~0),
         two_md = ifelse(two_att == 1 & md ==1,1,0),
         three_att = case_when(grepl('3pt',home)~1, T~0),
         three_md = ifelse(three_att == 1 & md ==1,1,0),
         ft_att = case_when(grepl('freethrow 1|freethrow 2|freethrow 3',home)~1,T~0),
         ft_md = ifelse(ft_att == 1 & md ==1,1,0),
         off_reb = case_when(grepl('rebound offensive',home)~1,T~0),
         def_reb = case_when(grepl('rebound defensive',home)~1,T~0),
         deadball = case_when(grepl('deadball',home)~1,T~0),
         off_reb = ifelse(off_reb == 1 & deadball == 1,0,off_reb),
         def_reb = ifelse(def_reb == 1 & deadball == 1,0,def_reb),
         to = case_when(grepl('turnover',home)~1, T~0),
         from = case_when(grepl('fromturnover',home)~1,T~0),
         to = ifelse(to == 1 & from == 1,0,to),
         ast = case_when(grepl('assist',home)~1,T~0),
         o_two_att = case_when(grepl('2pt',away)~1, T~0),
         o_md = case_when(grepl('made',away)~1,T~0),
         o_two_md = ifelse(o_two_att == 1 & o_md ==1,1,0),
         o_three_att = case_when(grepl('3pt',away)~1, T~0),
         o_three_md = ifelse(o_three_att == 1 & o_md ==1,1,0),
         o_ft_att = case_when(grepl('freethrow 1|freethrow 2|freethrow 3',away)~1,T~0),
         o_ft_md = ifelse(o_ft_att == 1 & o_md ==1,1,0),
         o_off_reb = case_when(grepl('rebound offensive',away)~1,T~0),
         o_def_reb = case_when(grepl('rebound defensive',away)~1,T~0),
         o_deadball = case_when(grepl('deadball',away)~1,T~0),
         o_off_reb = ifelse(o_off_reb == 1 & o_deadball == 1,0,o_off_reb),
         o_def_reb = ifelse(o_def_reb == 1 & o_deadball == 1,0,o_def_reb),
         o_to = case_when(grepl('turnover',away)~1, T~0),
         o_from = case_when(grepl('fromturnover',away)~1,T~0),
         o_to = ifelse(o_to == 1 & o_from == 1,0,o_to),
         o_ast = case_when(grepl('assist',away)~1,T~0),
         fg_md = two_md+three_md,
         fg_att = two_att+three_att,
         o_fg_md = o_two_md+o_three_md,
         o_fg_att = o_two_att+o_three_att) %>%
  select(half,qtr,fg_md,fg_att,three_md,three_att,ft_md,ft_att,off_reb,def_reb,to,ast,
         o_fg_md,o_fg_att,o_three_md,o_three_att,o_ft_md,o_ft_att,o_off_reb,o_def_reb,o_to,o_ast) %>%
  group_by(half,qtr) %>% summarise_all(sum) %>% ungroup()

h = box %>% filter(half <=2)%>% group_by(half) %>% summarise_all(sum) %>% ungroup()
o = box %>% summarise_all(sum)

a = as.data.frame(t(html_analysis[[2]]))
a = a %>% filter(row_number()==1) %>% separate_wider_delim(V4,delim = ' ',names = c("Date",'time','am/pm')) %>% 
  select(2:4,7) %>% 
  mutate(opponent = V2,team = V3,venue = V5, id = i) %>%
  separate_wider_delim(id,delim = '/', names = c('x1','x2','x3','x4','game_id','x6'),too_few = 'align_end') %>%
  select(team,opponent,Date,venue,game_id) 

box = rbind(box,h,o)
box = cbind(box,a)
box = box %>% select(1:26)

master_pbp = distinct(rbind(master_pbp,box))
write.csv(master_pbp,'C:/Users/ryann/OneDrive/Documents/R Projects/pbp.csv')
}