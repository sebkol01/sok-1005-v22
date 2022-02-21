library(rvest)
library(tidyverse)
library(janitor)

# Oppgave 1

lists <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

str(lists)

cars <- lists %>% html_table(fill=TRUE)

table <- cars[[1]]


table %>% 
  ggplot(aes(x=`wltp`, y=stopp))+
  geom_point()


names(table) <- c("Modell (temp. varierte fra 0° til -10°)", "WLTP", "STOPP", "Avvik")

table <- table [-1,]

table <- table %>% clean_names()

table <- table %>% 
  mutate(stopp = as.numeric(gsub("km", "", stopp)))

table <- table %>% 
  mutate(wltp = as.numeric(gsub("km.*", "", wltp)))

table %>% 
  select(wltp, stopp) %>% 
  ggplot(aes(x = wltp, y = stopp))+
  geom_point()+
  ggtitle("Driving range for Electric cars during winter")+
  ylab("stop")+
  xlab("WLTP range")+
  scale_x_continuous(limits = c(200, 600))+
  scale_y_continuous(limits = c(200, 600))+
  geom_abline(col = "blue", size = .5)+
  theme_bw()

# I figuren kan vi se hvor langt hver bil som var med i testen
# faktisk kunne kjøre i vinterkulda og vi ser hvor langt produsenten 
# har sagt de kan kjøre, altså den "egentlige" kjørelengden.
# Dette kan være fordi man bruker mer strøm på varme i bilen, som vil gjøre
# at bilen trekker mer strøm enn den ville gjort om man ikke bruker noe varme i bilen.

# Oppgave 2

lm(stopp ~ wltp, data = table)
