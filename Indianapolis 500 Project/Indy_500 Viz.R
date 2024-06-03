library(readxl)
library(tidyverse)
library(ggbump)
library(ggtext)
library(showtext)

Indy = read_excel("Indy 500.xlsx")

Indy = Indy %>% mutate(ifelse(Lap >= 1 & Lap <= 8,'C',
                              ifelse(Lap >= 22 & Lap <= 25,'C',
                                     ifelse(Lap >= 28 & Lap <= 31,'C',
                                            ifelse(Lap >= 56 & Lap <= 63,'C',
                                                   ifelse(Lap >= 86 & Lap <= 90,'C',
                                                          ifelse(Lap >= 107 & Lap <= 112,'C',
                                                                 ifelse(Lap >= 114 & Lap <= 117,'C',
                                                                        ifelse(Lap >= 147 & Lap <= 157,'C','G')))))))))
#  filter(Lap >=150)

font_add_google("Cairo", "cairo")
showtext_auto()

graphic = Indy %>%
  select(Driver = Driver_name,Lap,Pos) %>% mutate(fill_color = case_when(
    Driver == "Larson, Kyle" ~ '#ff7f00',
    .default = 'grey50'
  ),
  alpha = ifelse(Driver %in% 'Larson, Kyle', 1, .5)) %>%
  ggplot(aes(Lap,Pos, color = fill_color, group = Driver)) + 
  geom_point(size = 2.5, aes(alpha = alpha)) + 
  geom_bump(aes(alpha = alpha), linewidth = 1) + scale_color_identity() + 
  scale_x_continuous(breaks = c(1,seq(50,150,50),200),
                     labels = c('Lap 1', 'Lap 50','Lap 100', 'Lap 150','Checkered')) +
  scale_y_reverse(breaks = seq(1,33,1), labels = c('1st', seq(2,32,1),'Last')) +
  theme_void() + 
  theme(legend.position = 'none',
        plot.title.position = 'plot',
        plot.title = element_text(family = 'cairo', hjust = .5, size = 30, face = 'bold', vjust = 0),
        plot.subtitle = element_markdown(family = 'cairo', hjust = .5, size = 18),
        plot.caption.position = 'plot',
        plot.caption = element_text(family = 'cairo', hjust = 0.5, vjust = -2, size = 12),
        axis.text = element_text(family = 'cairo', size = 15),
        axis.text.x = element_text(vjust = -1),
        panel.grid.major.x = element_line(color = 'grey70', linetype = 'dotted'),
        panel.grid.major.y = element_line(color = 'grey70', linetype = 'dotted'),
        plot.margin = unit(c(.25, .5, .5, .5), "cm"),
        plot.background = element_rect(fill = "#F6F7F2")) +
  labs(title = '108th Running of the Indianapolis 500 Driver Positions',
       subtitle = "<p>How <span style ='color:#ff7f00;'><b>Kyle Larson</b></span> fared in his first Indy 500</p>",
       caption = "Data by INDYCAR || Viz. by @ryan_neudorff")
graphic
ggsave(plot = graphic, w = 15, h = 4.81, "Indy_500.png")
