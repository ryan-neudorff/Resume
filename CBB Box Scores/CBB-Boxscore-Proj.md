CBB Box Score Project
================
Ryan Neudorff

### Introduction

During the 2023-24 College Basketball season, I got the idea to keep
track of advance stats during Purdue games. So before Purdue’s game
against Xavier that season. I created a Google sheets that kept track of
the two teams four factors[^1], Points per Possession, and total
possessions for that half. And I did that for each of the remaining
games the Boilermakers played that season (including Postseason).

And overall it was really fun to see how a team performed on a half by
half basis. One half a team can look like world beaters, the other they
can be lucky just to scrape out a win. So entering this current season,
I wanted to update my “Advanced Box Scores” by making it automated in a
way. But I never got around to doing it. Fast forward to around
thanksgiving and I was in the process of learning how to webscrape and
always liked what Kenpom did for his box scores shown below in figure 1.

<center>

<figure>
<img
src="C:/Users/ryann/OneDrive/Documents/R%20Projects/RMarkdown%20files/images/Kenpom%20Box.png"
style="width:50.0%" alt="Fig 1" />
<figcaption aria-hidden="true">Fig 1</figcaption>
</figure>

</center>

So I figured why not use webscrapping to pull play by play data and show
how teams are doing per 10 minute intervals.

### Grabing our Data

To get started, we need to load in the required packages

``` r
library(tidyverse)
library(rvest)
library(openxlsx)
```

We then use the `rvest` package to scrape play by play data from the
NCAA.

``` r
html = read_html('https://stats.ncaa.org/contests/5731244/play_by_play')

x = html %>% html_nodes('table') %>%
  html_table()

w = x[[4]]
w = w %>% separate_wider_delim(.,cols=Time,delim = ':', names = c("min",'sec','mic')) %>%
  mutate_at(vars(min,sec,mic),as.numeric) %>%
  mutate(min = min*60, Time = min+sec, half = 1, qtr = case_when(half == 1 & Time >= 600 ~ 1,
                                                                 T ~ 2)) %>%
  select(half,qtr,4,6)
y = x[[5]]
y = y %>% separate_wider_delim(.,cols=Time,delim = ':', names = c("min",'sec','mic')) %>%
  mutate_at(vars(min,sec,mic),as.numeric) %>%
  mutate(min = min*60, Time = min+sec, half = 2, qtr = case_when(half == 2 & Time >= 600 ~ 3,
                                                                 T ~ 4)) %>%
  select(half,qtr,4,6)
#o1 = x[[6]]
#o1 = o1 %>% mutate(half = 3, qtr = 5) %>% select(half,qtr,2,4)
#o2 = x[[7]]
#o2 = o2 %>% mutate(half = 3, qtr = 5) %>% select(half,qtr,2,4)
#o3 = x[[8]]
#o3 = o3 %>% mutate(half = 3, qtr = 5) %>% select(half,qtr,2,4)
```

In the above block of code, we are using the `rvest` package to pull all
of the html tags that say table and putting them into a list. We can
then specify which list we want to pull, (in this case list numbers 4
and 5 contain the information that we need) then using the `dplyr`
package, to modify the data to get the half, the quarter, and the play
description[^2].

``` r
pbp = rbind(w,y)
#pbp = rbind(w,y,ot)
```

After modifying the individual datasets, we can use the `rbind` function
to combine them into one datatset called pbp[^3]

### Modifying our Data

once we combined all of our data in the previous step, We will use a
series of `case_when` and `grepl` functions to make binary indicators
that we can easily sum later in the code

``` r
box = pbp %>% mutate(away = .[[3]], home = .[[4]]) %>% select(half,qtr,away,home) %>%
  mutate(two_att = case_when(grepl('2pt',home)~1, T~0),
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
         game_id = 5731384,
         fg_md = two_md+three_md,
         fg_att = two_att+three_att,
         o_fg_md = o_two_md+o_three_md,
         o_fg_att = o_two_att+o_three_att) %>%
  select(game_id,half,qtr,fg_md,fg_att,three_md,three_att,ft_md,ft_att,off_reb,def_reb,to,ast,
         o_fg_md,o_fg_att,o_three_md,o_three_att,o_ft_md,o_ft_att,o_off_reb,o_def_reb,o_to,o_ast)
```

``` r
a = as.data.frame(t(x[[2]]))
a = a %>% filter(row_number()==1) %>% separate_wider_delim(V4,delim = ' ',names = c("Date",'time','am/pm')) %>% 
  select(2:4,7) %>% 
  mutate(opponent = V2,team = V3,venue = V5) %>%
  select(team,opponent,Date,venue) 
```

The last thing we do before outputting this code onto a excel
spreadsheet is we need to grab the totals for every quarter, half,
(overtime if needed).

``` r
quarter = box %>% filter(qtr <= 4) %>% group_by(game_id,qtr) %>% summarise_at(vars(2:21),sum) %>% ungroup() %>%
  select(-qtr) %>% mutate(pos = fg_att-off_reb+to+(.475*ft_att),
                          PPP = ((fg_md-three_md)*2+three_md*3+ft_md)/pos,
                          efg_pct =(.5*three_md+fg_md)/fg_att,
                          orb_pct = off_reb/(off_reb+o_def_reb),
                          to_pct = to/pos,
                          ftr = ft_att/fg_att,
                          ast_pct = ast/fg_md,
                          o_pos = o_fg_att-o_off_reb+o_to+(.475*o_ft_att),
                          o_PPP = ((o_fg_md-o_three_md)*2+o_three_md*3+o_ft_md)/o_pos,
                          o_efg_pct = (.5*o_three_md+o_fg_md)/o_fg_att,
                          o_orb_pct = o_off_reb/(o_off_reb+def_reb),
                          o_to_pct = o_to/o_pos, 
                          o_ftr = o_ft_att/o_fg_att,
                          o_ast_pct = o_ast/o_fg_md) %>%
  select(1,22:35)

half = box %>% filter(half <= 3) %>% group_by(game_id,half) %>% summarise_at(vars(2:21),sum) %>% ungroup() %>%
  select(-half) %>% mutate(pos = fg_att-off_reb+to+(.475*ft_att),
                           PPP = ((fg_md-three_md)*2+three_md*3+ft_md)/pos,
                           efg_pct =(.5*three_md+fg_md)/fg_att,
                           orb_pct = off_reb/(off_reb+o_def_reb),
                           to_pct = to/pos,
                           ftr = ft_att/fg_att,
                           ast_pct = ast/fg_md,
                           o_pos = o_fg_att-o_off_reb+o_to+(.475*o_ft_att),
                           o_PPP = ((o_fg_md-o_three_md)*2+o_three_md*3+o_ft_md)/o_pos,
                           o_efg_pct = (.5*o_three_md+o_fg_md)/o_fg_att,
                           o_orb_pct = o_off_reb/(o_off_reb+def_reb),
                           o_to_pct = o_to/o_pos, 
                           o_ftr = o_ft_att/o_fg_att,
                           o_ast_pct = o_ast/o_fg_md) %>%
  select(1,22:35)

overall = box %>% group_by(game_id) %>% summarise_at(vars(3:22),sum) %>%
  mutate(pos = fg_att-off_reb+to+(.475*ft_att),
         PPP = ((fg_md-three_md)*2+three_md*3+ft_md)/pos,
         efg_pct =(.5*three_md+fg_md)/fg_att,
         orb_pct = off_reb/(off_reb+o_def_reb),
         to_pct = to/pos,
         ftr = ft_att/fg_att,
         ast_pct = ast/fg_md,
         o_pos = o_fg_att-o_off_reb+o_to+(.475*o_ft_att),
         o_PPP = ((o_fg_md-o_three_md)*2+o_three_md*3+o_ft_md)/o_pos,
         o_efg_pct = (.5*o_three_md+o_fg_md)/o_fg_att,
         o_orb_pct = o_off_reb/(o_off_reb+def_reb),
         o_to_pct = o_to/o_pos, 
         o_ftr = o_ft_att/o_fg_att,
         o_ast_pct = o_ast/o_fg_md) %>%
  select(1,22:35)

# ot = box %>% filter(half == 3) %>% group_by(game_id) %>% summarise_at(vars(3:22),sum) %>%
#   mutate(pos = fg_att-off_reb+to+(.475*ft_att),
#          PPP = ((fg_md-three_md)*2+three_md*3+ft_md)/pos,
#          efg_pct =(.5*three_md+fg_md)/fg_att,
#          orb_pct = off_reb/(off_reb+o_def_reb),
#          to_pct = to/pos,
#          ftr = ft_att/fg_att,
#          ast_pct = ast/fg_md,
#          o_pos = o_fg_att-o_off_reb+o_to+(.475*o_ft_att),
#          o_PPP = ((o_fg_md-o_three_md)*2+o_three_md*3+o_ft_md)/o_pos,
#          o_efg_pct = (.5*o_three_md+o_fg_md)/o_fg_att,
#          o_orb_pct = o_off_reb/(o_off_reb+def_reb),
#          o_to_pct = o_to/o_pos,
#          o_ftr = o_ft_att/o_fg_att,
#          o_ast_pct = o_ast/o_fg_md) %>%
#   select(1,22:35)
```

And lastly, we combine them all into one dataset and export

``` r
box = rbind(quarter,half,overall)
box = cbind(box,a)


write.xlsx(box,".../Boxscores.xlsx", sheetName = "Data")
```

And to create the visualization I could have used the `gt` and
`gtextras` packages for the visualization, but my knowledge of html and
css isn’t great. And I have a bit more flexibility of creating the look
I want on Excel. So I take the exported data that is now in a excel
spreadsheet, use some cell references and XLOOKUP function to create the
final product below in figure 2.

<center>

<figure>
<img
src="C:/Users/ryann/OneDrive/Documents/R%20Projects/RMarkdown%20files/images/Screenshot%202025-02-20%20151158.png"
alt="Fig 2" />
<figcaption aria-hidden="true">Fig 2</figcaption>
</figure>

<center>

[^1]: Effective Shooting, Turnovers, Offensive Rebounds, and Free Throw
    Attempts. More can be found about
    [here](https://kenpom.com/blog/four-factors/)

[^2]: The area of the code that is commented out is for the instance
    that the game does not end in regulation. Every additional overtime
    period add another table that we need to pull out of the html code,
    and most games historically don’t go past 3 overtimes

[^3]: once again if the game we want to create a box score did not end
    in regulation, we would rbind (ot,ot2 … ot(n)) along with w and y
