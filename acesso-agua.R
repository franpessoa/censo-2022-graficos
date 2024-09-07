library(tidyverse)
library(janitor)
library(plotly)
library(stringr)
library(ggthemes)
library(htmlwidgets)

d <- read_csv("./dados/acesso-agua.csv") |> janitor::clean_names() |> 
  dplyr::rename(loc = x1) |> 
  mutate(
    across(
      starts_with("n") | contains("lig") | contains("encanada"), 
      function (x) str_replace_all(x, "[\\.\\s,]", "")
    )
  ) |>
  # convert to numeric
  mutate(
    across(
      starts_with("n") | contains("lig") | contains("encanada"), 
      as.numeric
    )
  ) |>
  pivot_longer(
    cols = 9:11,
    names_to = "tipo_encanamento",
    values_to = "qtd_tipo_encanamento"
  ) |>
  mutate(
    tipo_encanamento = case_when(
      tipo_encanamento == "encanada_casa" ~ "encanada até a casa",
      tipo_encanamento == "encanada_terreno" ~ "encanada até o terreno",
      tipo_encanamento == "n_encanada" ~ "não encanada"
    ),
    qtd_tipo_encanamento = qtd_tipo_encanamento / 10000
  )

p <- d |> 
  filter(loc == "Brasil") |>
  ggplot(aes(
    x = tipo_encanamento,
    y = qtd_tipo_encanamento,
    fill = tipo_encanamento,
    color = tipo_encanamento,
    alpha = 0.5
  )) +
  geom_bar(stat = "identity", width = 1) +
  theme_gray() +
  labs(
    title = "Tipo de acesso a água em comunidades quilombolas",
    subtitle = "IBGE - Censo de 2022; resultados do Brasil inteiro",
    x = "Número de casas",
    y = "Quantidade de residências (dezenas de milhar)",
    fill = "Categoria",
  ) + 
  guides(color = FALSE, alpha = FALSE, fill = FALSE)

ggsave(p, "plots/acesso-água.png", dpi = 450)
saveWidget(ggplotly(p, tooltip = "qtd_tipo_encanamento"), "plots/acesso-agua/index.html")
