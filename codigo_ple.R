
pacman::p_load(tidyverse,
               ggplot2,
               tm,
               text2vec,
               igraph,
               readxl,
               text2vec,
               tidytext,
               wordcloud2,
               stringi)

# Abrir bb.dd   

raw_data <- read_excel("BBDD.xlsx") %>% as.data.frame()

# Limpiar comentarios

clean_data <- raw_data %>%
  mutate(
    comentario = ifelse(is.na(comentario) | comentario == "", "Texto no disponible", comentario), # Manejar NAs
    comentario = tolower(comentario),              # Convertir a minúsculas
    comentario = chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", comentario), # Eliminar tildes
    comentario = gsub("[[:punct:]]", "", comentario), # Remover puntuación
    comentario = gsub("[[:digit:]]", "", comentario), # Remover números
    comentario = trimws(comentario)                # Eliminar espacios extra
  )

## gráfico alfileres

# Contar comentarios por noticia

df_comentarios <- clean_data %>%
  group_by(noticia) %>%
  summarise(comentario = n())

# Crear el gráfico de alfileres

grafico_comentarios <- ggplot(df_comentarios, aes(x = comentario, y = reorder(noticia, comentario))) +
  geom_segment(aes(x = 0, xend = comentario, y = noticia, yend = noticia), color = "darkblue") +
  geom_point(aes(x = comentario, y = noticia), color = "darkblue", size = 3) +
  geom_text(aes(x = comentario + 0.5, y = noticia, label = comentario), hjust = -1, color = "black") +
  labs(x = "Cantidad de comentarios", y = "Noticia") +
  theme_bw() + scale_x_continuous(expand = expansion(mult = c(0, 0.1))) 

### dividir bb.dd por noticia

data_llaitul <- clean_data %>% filter(clean_data$noticia == "Josefa Barraza")

data_venezuela <- clean_data %>% filter(clean_data$noticia == "Venezuela")

data_sandoval <- clean_data %>% filter(clean_data$noticia == "Francisca Sandoval")

# transformar bb.dd a corpus de texto

corpus_completo <- Corpus(VectorSource(clean_data$comentario))
corpus_llaitul <- Corpus(VectorSource(data_llaitul$comentario))
corpus_venezuela <- Corpus(VectorSource(data_venezuela$comentario))
corpus_sandoval <- Corpus(VectorSource(data_sandoval$comentario))

# limpiar el corpus con stopwords 

# declarar diccionario

stopwords_es <- c(stopwords("spanish"))

palabras_adicionales <- c("ser", "solo", "asi", "mas", "hace", "puede", "como", "dice", "hacer", "igual", "menos", "debe", "ahi" )

stopwords_es_cl <- c(stopwords_es, palabras_adicionales)

# tm_map

corpus_completo <- tm_map(corpus_completo, removeWords, stopwords_es_cl)

corpus_llaitul <- tm_map(corpus_llaitul, removeWords, stopwords_es_cl)
corpus_venezuela <- tm_map(corpus_venezuela, removeWords, stopwords_es_cl)
corpus_sandoval <- tm_map(corpus_sandoval, removeWords, stopwords_es_cl)

# transformar corpus a TDM

tdm_completo <- TermDocumentMatrix(corpus_completo) %>% as.matrix()
tdm_llaitul <- TermDocumentMatrix(corpus_llaitul) %>% as.matrix()
tdm_venezuela <- TermDocumentMatrix(corpus_venezuela) %>% as.matrix()
tdm_sandoval <- TermDocumentMatrix(corpus_sandoval) %>% as.matrix()

# Sumar frecuencias de cada una

word_freqs_total <- sort(rowSums(tdm_completo), decreasing = TRUE)

word_freqs_total <- data.frame(
  word = names(word_freqs_total),
  freq = as.numeric(word_freqs_total))

word_freqs_llaitul <- sort(rowSums(tdm_llaitul), decreasing = TRUE)

word_freqs_llaitul <- data.frame(
  word = names(word_freqs_llaitul),
  freq = as.numeric(word_freqs_llaitul))

word_freqs_venezuela <- sort(rowSums(tdm_venezuela), decreasing = TRUE)

word_freqs_venezuela <- data.frame(
  word = names(word_freqs_venezuela),
  freq = as.numeric(word_freqs_venezuela))

word_freqs_sandoval <- sort(rowSums(tdm_sandoval), decreasing = TRUE)

word_freqs_sandoval <- data.frame(
  word = names(word_freqs_sandoval),
  freq = as.numeric(word_freqs_sandoval))

# filtar 50 palabras más usadas 

word_freqs_total_filtered <- word_freqs_total %>%
  filter(freq >= 5) %>%     # Filtrar palabras con frecuencia >= 5
  arrange(desc(freq)) %>%   # Ordenar de mayor a menor frecuencia
  slice_head(n = 50)        # Limitar a las primeras 50 palabras

# filtro llaitul

word_freqs_llaitul_filtered <- word_freqs_llaitul %>%
  filter(freq >= 5) %>%     
  arrange(desc(freq)) %>%   
  slice_head(n = 50)        


# filtro venezuela

word_freqs_venezuela_filtered <- word_freqs_venezuela %>%
  filter(freq >= 5) %>%     
  arrange(desc(freq)) %>%   
  slice_head(n = 50)        

# filtro sandoval

word_freqs_sandoval_filtered <- word_freqs_sandoval %>%
  filter(freq >= 5) %>%     
  arrange(desc(freq)) %>%   
  slice_head(n = 50)        

# Crear las nubes

words_total <- wordcloud2(data = word_freqs_total_filtered, color = "random-dark", shape = "pentagon")
words_llaitul <- wordcloud2(data = word_freqs_llaitul_filtered, color = "random-dark", shape = "pentagon")
words_venezuela <- wordcloud2(data = word_freqs_venezuela_filtered, color = "random-dark", shape = "pentagon")
words_sandoval <- wordcloud2(data = word_freqs_sandoval_filtered, color = "random-dark", shape = "pentagon")

# transformacion corpus

corpus_completo_grafo <- data.frame(text = sapply(corpus_completo, as.character), stringsAsFactors = FALSE) %>% as_tibble

# Tokenización en bigramas (pares de palabras consecutivas)

bigramas <- corpus_completo_grafo %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)


# Separar bigramas en palabra1 y palabra2

bigramas_separados <- bigramas %>% separate(bigram, into = c("palabra1", "palabra2"), sep = " ")

# Paso 2: Seleccionar las 15 palabras más usadas

palabras_mas_usadas <- bigramas_separados %>%
  count(palabra1, sort = TRUE) %>%
  top_n(200, n) %>%
  pull(palabra1)

# Paso 3: Contar la frecuencia de bigramas

frecuencia_bigramas <- bigramas_separados %>%
  count(palabra1, palabra2, sort = TRUE)


# Filtrar bigramas con las palabras más usadas

red_bigramas <- frecuencia_bigramas %>%
  filter(palabra1 %in% palabras_mas_usadas)

umbral_frecuencia <- 5# Cambia este valor según la densidad que desees

red_bigramas_filtrada <- red_bigramas %>%
  filter(n >= umbral_frecuencia)%>% drop_na()

# Crear el grafo con la red filtrada

grafo_filtrado <- graph_from_data_frame(red_bigramas_filtrada)

# Configuración de los tamaños de nodos y aristas

vertex_sizes <- degree(grafo_filtrado, mode = "all") * 10  # Tamaño según conexiones

edge_widths <- E(grafo_filtrado)$n / max(E(grafo_filtrado)$n) * 4  # Grosor según frecuencia

# Ajuste de la distribución y visualización

layout_grafo <- layout_with_kk(grafo_filtrado)

# Visualizar el grafo
grafo_total <- plot(grafo_filtrado,
     layout = layout_grafo,
     vertex.size = vertex_sizes,       # Tamaño de los nodos
     vertex.label.cex = 1.2,           # Tamaño del texto en los nodos
     vertex.label.color = "black", 
     
     edge.width = edge_widths,         # Grosor de las aristas
     edge.color = "#D1D1D1",         # Color de las arista
     vertex.label.family = "arial",
     vertex.frame.color = "#D1D1D1",
     vertex.color = "white")



# Sumar frecuencias de cada término

word_freqs_llaitul <- sort(rowSums(tdm_llaitul), decreasing = TRUE) %>% as.data.frame() %>% rownames_to_column("word")
word_freqs_venezuela <- sort(rowSums(tdm_venezuela), decreasing = TRUE) %>% as.data.frame() %>% rownames_to_column("word")
word_freqs_sandoval <- sort(rowSums(tdm_sandoval), decreasing = TRUE) %>% as.data.frame() %>% rownames_to_column("word")




