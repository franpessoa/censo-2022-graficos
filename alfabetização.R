library(tidyverse)
library(janitor)
library(stringr)
library(plotly)
library(htmlwidgets)

a <- read_csv("./dados/alfabetização-quilombos.csv") |> 
  janitor::clean_names() |> 
  mutate(across(2:5, function (x) str_replace_all(x, "\\.", ""))) |> 
  mutate(across(2:5, as.numeric)) |> 
  mutate(
    alf_geral = (pma15_total_alfabetizada / pma15_total) * 100, 
    alf_quilombols = (pma15_quilombola_alfabetizada / pma15_quilombola_total) * 100,
    tipo = ifelse(
      uf_regiao == "Brasil", 
      yes = "País", 
      no = ifelse(
        uf_regiao %in% c("Norte", "Nordeste", "Sul", "Sudeste", "Centro-Oeste"),
        yes = "Região",
        no = "Estado"
      )
  ))

p <- a |> ggplot(aes(
    x = alf_geral, 
    y = alf_quilombols, 
    size = pma15_total,
    text = uf_regiao,
    color = tipo
  )) +
  geom_point() +
  coord_cartesian(xlim = c(75, 100), ylim = c(65,100)) +
  labs(
    title = "Taxa de alfabetização nos estados vs nas comunidades quilombolas",
    x = "Porcentagem de alfabetizados no estado",
    y = "Porcentagem de alfabetizados nas comunidades quilombolas",
    size = "População",
    color = "Tipo de Medida",
  ) +
  theme_gray() +
  theme(
    plot.title = element_text(face = "bold")
  )
  
ggsave("plots/alfabetização.png", plot = p, dpi = 450)
p_i <- ggplotly(p)

saveWidget(p_i, "plots/alfabetização/index.html")