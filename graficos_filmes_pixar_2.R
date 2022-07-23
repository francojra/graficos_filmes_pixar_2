# Base de dados - Curso R ------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 23/07/22 ---------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(dados)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(forcats)
library(gridExtra)

# Selecionar dados -------------------------------------------------------------------------------------------------------------------------

pix <- dplyr::glimpse(dados::pixar_avalicao_publico)
pix1 <- dplyr::glimpse(dados::pixar_bilheteria)
pix2 <- dplyr::glimpse(dados::pixar_oscars)

left_join <- left_join(pix, pix2, by = "filme")
dim(left_join)
View(left_join)
fp <- left_join

# Análises ---------------------------------------------------------------------------------------------------------------------------------

fp <- fp %>%
  drop_na()
View(fp)

g1 <- ggplot(fp, aes(x = fct_reorder(filme, nota_rotten_tomatoes), 
             y = nota_rotten_tomatoes)) +
  geom_col(fill = "#66c2a5") +
  labs(x = "Filmes", y = "Notas Rotten Tomatoes") +
  theme(axis.text.x = element_text(size = 12, color = "black", 
                                 angle = 40),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 15, color = "black")) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  coord_flip()
g1

