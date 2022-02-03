library(rjson)
library(tidyverse)
library(ggrepel)
library(formattable)

# OPPGAVE 1

list <- fromJSON(file= "https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")

str(list)



df <- do.call(rbind, list) %>% 
  as.data.frame 


dfnum <- as.numeric(df$deaths, df$fully_vaccinated_pct_of_pop, df$name)


df$fully_vaccinated_pct_of_pop <- as.numeric(df$fully_vaccinated_pct_of_pop)


class(df$fully_vaccinated_pct_of_pop)

df$deaths <- as.numeric(df$deaths)


df$deaths_per_100k <- as.numeric(df$deaths_per_100k)

plot <- df %>% 
  ggplot(aes(x=fully_vaccinated_pct_of_pop, y=deaths_per_100k, label = name))+
  geom_point(col = "green4")+
  geom_label_repel(max.overlaps = Inf,
                   size =3.5,
                   label.size = 0,
                   label.padding = 0.1,
                   label.r = 0,
  )+
  scale_y_continuous()+
  scale_x_continuous(labels=scales::percent)+
  labs(title = "Covid-19 deaths since universal adult vaccine eligibility compared with vaccination rates",
       x = "Share of total population fully vaccinated",
       y = " ")+
  theme_bw()


# OPPGAVE 2
df %>% 
  ggplot(aes(x=fully_vaccinated_pct_of_pop, y=deaths_per_100k, label = name))+
  geom_point(col = "green4")+
  geom_label_repel(min_segment_length = 0, 
                   max.overlaps = Inf,
                   size =3.5,
                   label.size = 0,
                   label.padding = 0.1,
                   label.r = 0,
  )+
  geom_smooth(method = "lm")+
  scale_y_continuous()+
  scale_x_continuous(labels=scales::percent, breaks = seq(from = .45, to = .80, by = 0.05))+
  labs(title = "Covid-19 deaths since universal adult vaccine eligibility compared with vaccination rates",
       x = "Share of total population fully vaccinated",
       y = " ")+
  theme_bw()



lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = df)


