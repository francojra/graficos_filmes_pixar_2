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

fp1 <- fp %>%
  group_by(resultado) %>%
  summarise(media = mean(nota_rotten_tomatoes),
            sd = sd(nota_rotten_tomatoes)) %>%
  drop_na()
View(fp1)
glimpse(fp1)
fp1$resultado <- as.factor(fp1$resultado)

g1 <- ggplot(fp1, aes(x = resultado, 
             y = media)) +
  geom_col(fill = "#af8dc3") +
  geom_errorbar(aes(x = resultado, 
             y = media, ymin = media - sd, ymax = media + sd),
             size = 0.85, width = 0.3) +
  labs(x = "Resultados", y = "Notas Rotten Tomatoes") +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 15, color = "black")) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))
g1

fp2 <- fp %>%
  group_by(tipo_premio_indicado) %>%
  summarise(media = mean(nota_rotten_tomatoes),
            sd = sd(nota_rotten_tomatoes)) %>%
  drop_na()
View(fp2)

g2 <- ggplot(fp2, aes(x = fct_reorder(tipo_premio_indicado, media),
                       media)) +
  geom_col(fill = "#7fbf7b") +
  geom_errorbar(aes(x = tipo_premio_indicado, 
             y = media, ymin = media - sd, ymax = media + sd),
             size = 0.85, width = 0.3) +
  labs(x = "Filmes", y = "Notas Rotten Tomatoes") +
  theme(axis.text.x = element_text(size = 12, color = "black", 
                                 angle = 40),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 15, color = "black")) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))
g2
