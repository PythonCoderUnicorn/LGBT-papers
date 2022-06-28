

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


# genders = genders %>% 
#   transmute(Transgender_ID = sample_characteristics),
#          count = as.numeric(x4) 
#          )


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



# ---------

df %>% 
  select(sample_characteristics, x4) %>% 
  slice(40:50) %>% # sex orientation
  transmute(sexual_orientation = sample_characteristics),
         count = as.numeric(x4)) %>% 
  ggplot(
    aes(x= count,
        y= fct_reorder(sexual_orientation, count),
        fill= count)
  )+
  geom_col()+
  ggdark::dark_mode() +
  scale_fill_paletteer_c(`"grDevices::rainbow"`)+
  coord_cartesian(xlim = c(0, 150), expand = F)+
  labs(
    title = "\nLGBT people who ever had a pregnancy",
    subtitle = "n = 210 in US",
    x="\nnumber reported ever having had a pregnancy\n",
    y="",
    caption = "\n @StarTrek_Lt | June 28, 2022 | Data: Moseson et al, 2020, IJTH DOI:10.1080/26895269.2020.1841058"
  )+
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




# ----------
# trans & preg
df2 %>% 
  select(pregnancy_history, n) %>% 
  slice(2:11) %>% # num of pregnancies
  transmute(num_pregnancies = pregnancy_history,
            count = as.numeric(n)) %>% 
  ggplot(
    aes(x= count, 
        y= num_pregnancies,
        fill= count)
  )+
  geom_col()+
  ggdark::dark_mode()



# --------
glimpse(df3)

# preg_status_overall= 
# preg_testosterone_use 
# preg_testosterone_use = 
# preg_testosterone = 

df3 %>% 
  select(x1,x11) # %>% 
  transmute(preg_currently = x1,
            testosterone_use = x10,
            testosterone_use= str_remove_all(testosterone_use, "[:punct:]"),
            testosterone_use= str_remove(testosterone_use, "38"),
            testosterone_use= str_remove(testosterone_use, "62"),
            testosterone_use= str_remove(testosterone_use, "40"),
            testosterone_use= str_remove(testosterone_use, "0$"),
            # testosterone_use= str_remove(testosterone_use, "21"),
            testosterone_use= str_remove(testosterone_use, "0$"),
            testosterone_use= str_remove(testosterone_use, "[:space:]")
            
         ) %>% 
  slice(3:7)


preg_status_overall
preg_miscarriage
preg_abortion = preg_testosterone_use
preg_abortion
preg_testosterone

df_1 = preg_status_overall %>% 
  inner_join(preg_miscarriage,
             preg_abortion,
             by="preg_currently"
             )
df_1 = df_1 %>% 
  inner_join(preg_testosterone, by="preg_currently")

df_1








# ------------
df4 = read_csv('trans-preg-4.csv') %>% clean_names()

glimpse(df4)



# preg_testost_ethnicity = 
df4.1 = df4 %>% 
  select(x,c(x2,x4,x6,x8,x10,x12,x14,x16,x18)) %>% 
  transmute(preg_outcome = factor(x),
            overall = x2,
            Amer_Indian_Native = x4,
            Asian = x6,
            Black = x8,
            Hispanic_Latinx = x10,
            Middle_East_N.Afr = x12,
            Hawiian_Pac_Island = x14,
            White = x16,
            Mult_racial = x18
  ) %>% 
  slice(16:23) %>% 
  mutate(Amer_Indian_Native = as.numeric(Amer_Indian_Native),
         Asian = as.numeric(Asian),
         Black = as.numeric(Black),
         Hispanic_Latinx = as.numeric(Hispanic_Latinx),
         Hawiian_Pac_Island = as.numeric(Hawiian_Pac_Island),
         Mult_racial = as.numeric(Mult_racial),
         White = as.numeric(White),
         overall = as.numeric(overall)
  )

df4.1 %>% 
  select(overall) %>% 
  sum()

df4.1 %>% 
  ggplot(
    aes(x= overall,
        y= fct_reorder(preg_outcome, overall),
        fill= overall
        )
  )+
  geom_col()+
  scale_fill_paletteer_c(`"grDevices::cm.colors"`)+
  ggdark::dark_mode()+
  coord_cartesian(xlim = c(0, 180), expand = F)+
  labs(
    title = "\nOverall Transgender pregnancy outcomes by Ethnicity",
    subtitle = " n= 472 in US (2020)",
    y= "Pregnancy Outcome",
    x="count",
    fill="count",
    caption = "\n @StarTrek_Lt | June 28, 2022 | Data: Moseson et al, 2020, IJTH DOI:10.1080/26895269.2020.1841058"
  )+
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










