

library(tidyverse)
library(glue)
library(janitor)
library(paletteer)
library(showtext)
library(ggtext) ## <<<<< !!!!


font_add_google('Lato','Lato')
showtext_auto()

df = read_csv('trans-preg-1.csv') %>% clean_names()

df2 = read_csv('trans-preg-2.csv') %>% clean_names()
df3 = read_csv('trans-preg-3.csv') %>% clean_names()

glimpse(df)



genders = df %>% 
  select(sample_characteristics, x4) %>% 
  slice(16:28)


genders = genders %>% 
  transmute(Transgender_ID = factor(sample_characteristics),
         count = as.numeric(x4) 
         )


genders %>% 
  arrange(desc(count))

genders %>% 
  ggplot(
    aes(x = count,
        y= fct_reorder(Transgender_ID,count),
        fill= count
        )
  ) +
  geom_col() +
  coord_cartesian(xlim = c(0, 120), expand = F)+
  labs(
    title = "\nTransgender people who ever had a pregnancy",
    subtitle = "n = 210 in US",
    x="\nnumber reported ever having had a pregnancy\n",
    y="",
    caption = "\n @StarTrek_Lt | June 28, 2022 | Data: Moseson et al, 2020, IJTH DOI:10.1080/26895269.2020.1841058"
  )+
  ggdark::dark_mode()+
  scale_fill_paletteer_c(`"grDevices::Warm"`)+
  theme(
    text = element_text(family = 'Lato'),
    plot.caption = element_textbox(face = 'italic', 
                                   color='grey70',
                                   hjust = 1,
                                   size = 11
                                   ),
    plot.title = element_textbox(size = 15, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size=13),
    axis.text.y = element_text(color = 'white', size=12),
    axis.text.x = element_text(color = 'white'),
    plot.margin = margin(t=1,r=1,b=1, 0, unit = 'cm')
  )













