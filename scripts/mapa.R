# Pacotes
library(ggplot2)   
library(sf)        # Trabalhar com dados espaciais (shapefiles)
library(dplyr)     # Para manipulação de dados (filtros, mutates, pipes)
library(ggspatial) # Adicionar escala e flecha norte 
library(geobr)     # Baixar dados geográficos oficiais do Brasil 
library(readxl)   
library(grid)      # Para funções gráficas, aqui usado para unidade de medida (unit())
library(ggrepel)   # adicionar rótulos (labels) nos gráficos ggplot2

# 1. Carregar shapefile municípios do Brasil (2020)
br_municipios <- read_municipality(year = 2020)  # geobr para baixar o polígono dos municípios do Brasil do ano 2020


# 2. Filtrar apenas municípios do Ceará
ceara <- br_municipios %>% filter(abbrev_state == "CE")


# 3. Carregar planilha com nomes das cidades
dados_amostras <- read_excel("dados_amostras.xlsx")


# 4. Selecionar a coluna municipios e corrigir os nomes
# Cria nova coluna Local_corrigido corrigindo nomes que estejam com acento errado ou faltando.
dados <- dados_amostras %>%
  select(municipio) %>%
  mutate(Local_corrigido = case_when(
    municipio == "Sao Benedito" ~ "São Benedito",
    municipio == "Quixere" ~ "Quixeré",
    TRUE ~ municipio
  ))


# 5. Criar vetor de nomes tratados da planilha (sem acentos e maiúsculo)
# trimws() remove espaços em branco antes/depois.
# toupper() deixa tudo em maiúsculas.
nomes_planilha <- toupper(trimws(dados$Local_corrigido)) %>%
  gsub("Á", "A", .) %>%
  gsub("Í", "I", .) %>%
  gsub("É", "E", .) %>%
  gsub("Ô", "O", .) %>%
  gsub("Ç", "C", .)


# 6. Criar coluna no shapefile para nome tratado (sem acentos e maiúsculo)
ceara_marcado <- ceara %>%
  mutate(nome_upper = toupper(name_muni) %>%
           gsub("Á", "A", .) %>%
           gsub("Í", "I", .) %>%
           gsub("É", "E", .) %>%
           gsub("Ô", "O", .) %>%
           gsub("Ç", "C", .) %>%
           trimws()
  ) %>%
  mutate(destacar = nome_upper %in% nomes_planilha)


# 7. Reprojetar para UTM (zona 24S - EPSG 31984, para Ceará)
ceara_marcado_utm <- st_transform(ceara_marcado, 31984)


# Criar objeto apenas com os municípios destacados
dados_destacados <- ceara_marcado_utm %>% filter(destacar)


# Extrair as coordenadas dos centroides
centroides <- st_centroid(dados_destacados)
centroides_df <- cbind(
  as.data.frame(centroides),
  st_coordinates(centroides)
)


# Agora o gráfico usando X e Y diretamente
ggplot(data = ceara_marcado_utm) + 
  geom_sf(aes(fill = destacar), color = "gray70", size = 0.1) +
  scale_fill_manual(values = c("TRUE" = "orange", "FALSE" = "gray90"), guide = "none") +
  geom_text_repel(
    data = centroides_df,
    aes(x = X, y = Y, label = name_muni),
    size = 3,
    max.overlaps = 100,
    segment.size = 0.2
  ) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm"),
    pad_x = unit(0.75, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.title.position = "plot") +
  labs(
    title = "Municípios de origem das amostras no Ceará",
    caption = "Fonte: IBGE / geobr"
  )


ggsave("mapa_amostras_ceara.pdf", 
       width = 12, height = 9, units = "in", 
       device = cairo_pdf)
