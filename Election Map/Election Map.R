library(tidyverse)
library(ggplot2)
library(ggmap)
library(sf)
library(maps)
library(classInt)

x = read.csv("C:/Users/ryann/Downloads/Voting_District_Boundaries_2023.csv")
y = x$p23
dat = data.frame(p23 = y,yes = c(58,12,29,6,20,17,6,50,1,21,18), no = c(509,211,274,180,376,553,87,748,137,421,263))
dat = dat %>% mutate(yes_pct = yes/(yes+no),no_pct = 1-yes_pct, margin = no_pct-yes_pct) %>%
  select(p23,margin)


my_sf = read_sf("C:/Users/ryann/Downloads/Voting_District_Boundaries_2023/Voting_District_Boundaries_2023.shp")
my_sf = merge(my_sf,dat,by='p23')

bks_qt = classIntervals(c(min(my_sf$margin)- .00001, my_sf$margin), n =4, style = 'quantile')

my_sf %>% mutate(margin_cat = cut(margin, bks_qt$brks),ID = row_number()) %>% 
  ggplot()+
  geom_sf(aes(fill=margin_cat),color='black')+
  geom_sf_text(aes(label = ID), size = 5, color = 'black', check_overlap = T, fontface = 'bold')+
  theme_void()+
  scale_fill_brewer(name = 'No', palette = "Oranges",labels= c('80%-85%','85%-88%','88%-91%','91%-99%'))+
  labs(
    title = '2025 Silver Creek Special Election',
    subtitle = '"Yes" or "No" Voting Margin by Precincts',
    caption = 'Data: Clark County, IN | Viz by: @ryan_neudorff'
  )+
  theme(
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background =  element_rect(fill = 'white', colour = NA),
    legend.position = 'right',
    legend.box = 'vertical',
    legend.title = element_text(size = 13.5),
    legend.title.align = .5,
    legend.key.size = unit(.25, 'in'),
    legend.key.spacing.y  = unit(.1,'in'),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(hjust = 0, size = 10, margin = margin(t = 10))
  )

ggsave('C:/Users/ryann/OneDrive/Documents/R Projects/Graphic/SCCE.png', width = 7,height = 7, units = 'in', dpi = 600)

