# pacotes -----------------------------------------------------------------

library(tidyverse)
library(dados)
library(gghighlight)
library(ggalt)
library(ggridges)
library(patchwork)
library(gganimate)

# anotações e realce ------------------------------------------------------

dados_starwars


# gghighlight -------------------------------------------------------------


p1 <- dados_starwars |> 
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  gghighlight::gghighlight(
    massa > 1000,
    label_key = nome, 
    unhighlighted_colour = "darkgreen"
  )


# ggalt -------------------------------------------------------------------

p2 <- dados_starwars |> 
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  ggalt::geom_encircle(
    data = filter(dados_starwars, massa > 1000),
    color = "red",
    s_shape = 0,
    expand = 0,
    spread = .02,
    size = 2
  )

dados_starwars |> 
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  ggalt::geom_encircle(
    data = filter(dados_starwars, altura > 220),
    color = "red",
    s_shape = 0,
    expand = 0.05,
    spread = 1,
    size = 2
  )


# ggrepel -----------------------------------------------------------------

p3 <- dados_starwars |> 
  ggplot() +
  aes(massa, altura) +
  geom_point() +
  ggrepel::geom_label_repel(
    data = filter(dados_starwars, altura > 220),
    aes(label = nome),
    color = "red"
  )


# ggridges ----------------------------------------------------------------

diamante |> 
  ggplot() +
  aes(preco, fill = corte) +
  geom_density(alpha = .4)

p4 <- diamante |> 
  ggplot() +
  aes(preco, corte, fill = corte) +
  ggridges::geom_density_ridges()


# patchwork ---------------------------------------------------------------

p1 + p2 + p3 + p4
wrap_plots(p1, p2, p3, p4, ncol = 4)


# gganimate ---------------------------------------------------------------

dados_gapminder |> 
  ggplot() +
  aes(
    expectativa_de_vida, 
    log10(pib_per_capita), 
    size = log10(populacao)
  ) +
  geom_point() +
  facet_wrap(~continente) +
  labs(
    title = "Ano : {frame_time}",
    x = "Expectativa de vida",
    y = "log10(PIB per capita)"
  ) +
  gganimate::transition_time(ano)

# gganimate::animate(nframes = 40)