# https://infogram.com/app/#edit/e8c54074-8b28-4564-a3d4-8dccd516b1d8
# https://contarcondatos.mincyt.gob.ar/descargas/DATOS2022_bases-y-condiciones.pdf


library(tidyverse)
library(waffle)

soja <- read_csv("data/soja-anual-1969-2020.csv")
centeno <- read_csv("data/centeno-anual-1923-2020.csv")
girasol <- read_csv("data/girasol-anual-1969-2019.csv")
avena <- read_csv("data/avena-anual-1923-2020.csv")
mijo <- read_csv("data/tabla-mijo-anual-1935-2019.csv")
maiz <- read_csv("data/maiz-serie-1923-2019-anual.csv")
trigo <- read_csv("data/trigo-serie-1923-2020-anual.csv")

data <- left_join(soja, maiz, by = "indice_tiempo") %>% 
  left_join(., trigo, by = "indice_tiempo") %>% 
  select("indice_tiempo", starts_with("superficie_sem")) %>% 
  rename(tiempo = indice_tiempo,
         soja = superficie_sembrada_soja_ha,
         maiz = superficie_sembrada_maiz_ha,
         trigo = superficie_sembrada_trigo_ha) %>% 
  mutate(tot = soja + maiz + trigo,
         soja = round(100*soja / tot),
         maiz = round(100*maiz / tot),
         trigo = 100-soja-maiz) %>% 
  pivot_longer(., c("soja", "maiz", "trigo")) %>% 
  select(name, value, tiempo)  %>% 
  filter(tiempo %in% c(1970,1990,2010) )


ggplot(data, aes(fill=name, values=value)) +
  geom_waffle(color = "white", size=1.125, n_rows = 10, ) +
  facet_wrap(~tiempo, nrow=1) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "Faceted Waffle Geoms"
  ) +
  theme(plot.caption = element_text(family = "Arial")) + 
  theme_enhance_waffle()




sup.pba.ha = 76002449
sup.caba.ha = 49421
mill = 1e6


left_join(soja, maiz, by = "indice_tiempo") %>% 
  left_join(., trigo, by = "indice_tiempo") %>% 
  select("indice_tiempo", starts_with("superficie_sem")) %>% 
  rename(tiempo = indice_tiempo,
         soja = superficie_sembrada_soja_ha,
         maiz = superficie_sembrada_maiz_ha,
         trigo = superficie_sembrada_trigo_ha) %>% 
  mutate(soja = soja/mill,
         maiz = maiz/mill,
         trigo = trigo/mill) %>% 
  pivot_longer(., c("soja", "maiz", "trigo")) %>% 
  select(name, value, tiempo) %>% 
  ggplot(aes(x = tiempo, y = value, color = name)) + geom_line()



