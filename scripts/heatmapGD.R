library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

####### heatmap ########

dados <- read_excel("reads_família.xls")

# ajuste dos dados, transf em fator
dados <- dados %>%
  mutate(
    Família = str_squish(Família),
    Pool = factor(Pool, levels = c(1, 2, 3, 4))
  )


# Somar reads por Pool e Família
dados_sum <- dados %>%
  group_by(Pool, Família) %>%
  summarise(
    NbReads = sum(NbReads, na.rm = TRUE),
    .groups = "drop"
  )


# Completar combinações ausentes (0 reads)
mat <- dados_sum %>%
  complete(Pool, Família, fill = list(NbReads = 0))

# Adicionar o total de reads virais de cada pool
# Valores correspondentes a "Mapped back to contigs"
mat <- mat %>%
  mutate(
    TotalReadsVirais = case_when(
      Pool == "1" ~ 499296,
      Pool == "2" ~ 277280,
      Pool == "3" ~ 339479,
      Pool == "4" ~ 442197
    )
  )

# Normalizar os reads por milhão de reads virais
mat <- mat %>%
  mutate(
    RPM_viral = (NbReads / TotalReadsVirais) * 1000000
  )

# Ordenar famílias pela abundância normalizada total
mat <- mat %>%
  group_by(Família) %>%
  mutate(total = sum(RPM_viral, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Família = reorder(Família, total))


# Heatmap em escala log10
ggplot(mat, aes(
  x = Pool,
  y = Família,
  fill = log10(RPM_viral + 1)
)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c(
      "#5e4fa2",
      "#3288bd",
      "#66c2a5",
      "#fee08b",
      "#d53e4f"
    ),
    name = "RPM viral",
    breaks = log10(
      c(0, 10, 100, 1000, 10000, 100000, 1000000) + 1
    ),
    labels = c(
      "0",
      "10",
      expression(10^2),
      expression(10^3),
      expression(10^4),
      expression(10^5),
      expression(10^6)
    )
  ) +
  labs(
    x = "Pool",
    y = "",
    title = "Heatmap de abundância de reads por família viral e por pool",
    subtitle = "Reads normalizados por milhão de reads virais"
  ) +
  coord_fixed(ratio = 0.25) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    axis.title = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )





