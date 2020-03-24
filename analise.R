library(tidyverse)
theme_set(theme_bw())
library(lubridate)
library(plotly)
library(brazilmaps)

############################
### preparacao dos dados ###
############################

# dados obtidos partir do site brasil.io:
# 
# https://brasil.io/dataset/covid19/caso

corona <- read.csv(file="https://brasil.io/dataset/covid19/caso?format=csv")
corona <- as_tibble(corona)

# transformar coluna `date` em data

corona <- corona %>% 
  mutate(date = ymd(date))

# transformar a coluna `is_last` em logical

corona <- corona %>% 
  mutate(is_last = ifelse(is_last == "True", TRUE, FALSE))

# remover as linhas com totais por cidade

corona <- corona %>% 
  filter(place_type == "state")

######################################
### analise exploratoria dos dados ###
######################################

hoje <- ymd("2020-03-22")

corona %>%
  filter(date == hoje) %>%
  arrange(desc(confirmed)) %>%
  select(state, confirmed) %>%
  print(n = Inf)

g <- corona %>%
  filter(state %in% c("SP", "RJ", "MG", "RS", "RN")) %>%
  group_by(date) %>%
  #top_n(1, confirmed) %>%
  ggplot(., aes(x = date, y = confirmed, group = state, colour = state)) +
  geom_line() +
  labs(x = "Data", y = "Casos Confirmados", colour = "Estado") +
  scale_colour_viridis_d()

ggplotly(g)



######################################
### analise exploratoria dos dados ###
######################################

# adiciona uma coluna chamada `estado`, com o nome do estado por extenso

names(get_brmap("State"))

codigos <- structure(list(codigo = c(11L, 12L, 13L, 14L, 15L, 16L, 17L, 
                                     21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 31L, 32L, 33L, 35L, 
                                     41L, 42L, 43L, 50L, 51L, 52L, 53L), estado = structure(c(22L, 
                                                                                              1L, 4L, 23L, 14L, 3L, 27L, 10L, 18L, 6L, 20L, 15L, 17L, 2L, 26L, 
                                                                                              5L, 13L, 8L, 19L, 25L, 16L, 24L, 21L, 12L, 11L, 9L, 7L), .Label = c("Acre", 
                                                                                                                                                                  "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal", 
                                                                                                                                                                  "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul", 
                                                                                                                                                                  "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", 
                                                                                                                                                                  "Piauí", "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", 
                                                                                                                                                                  "Rondônia", "Roraima", "Santa Catarina", "São Paulo", "Sergipe", 
                                                                                                                                                                  "Tocantins"), class = "factor"), uf = structure(c(21L, 1L, 3L, 
                                                                                                                                                                                                                    22L, 14L, 4L, 27L, 10L, 17L, 6L, 20L, 15L, 16L, 2L, 25L, 5L, 
                                                                                                                                                                                                                    11L, 8L, 19L, 26L, 18L, 24L, 23L, 12L, 13L, 9L, 7L), .Label = c("AC", 
                                                                                                                                                                                                                                                                                    "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", 
                                                                                                                                                                                                                                                                                    "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", 
                                                                                                                                                                                                                                                                                    "SC", "SE", "SP", "TO"), class = "factor")), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                                                     -27L))

mapa_br <- get_brmap("State")

mapa_br %>%
  left_join(codigos, c("State" = "codigo")) %>%
  left_join(filter(corona, date == hoje), c("uf" = "state")) %>%
  ggplot() +
  geom_sf(aes(fill = confirmed_per_100k_inhabitants)) +
  labs(fill = "Casos por 100k habitantes") +
  scale_fill_viridis_c(na.value = "grey") +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position = "bottom")


