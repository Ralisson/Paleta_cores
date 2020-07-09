### Projeto de criação de paleta de cores com base em foto

## necessário fazer o upload da foto e colocar no mesmo diretório e com o mesmo nome do arquivo "foto.jpg"




install.packages('tidyverse')
install.packages('jpeg')
install.packages("magrittr")
install.packages("dplyr")

library(jpeg)
library(magrittr)
library(tidyverse)
library(dplyr)
library(tibble)


img <- readJPEG("foto.jpg") #imagem é carregada como um array de 3 bandas (RBG) 


plot(as.raster(img)) # plotagem do array transformando novamente em imagem

img_matrix <- apply(img, 3, as.numeric)  # matriz principal com o RGB

#escolhe a quantidade de cores da paleta

km <- kmeans(img_matrix, centers = 6) # matriz principal com clusters - definir numeros de clusters.

# Cria tabela com resumo dos centroides

img_df <- tibble(
  r = img_matrix[,1], 
  g = img_matrix[,2], 
  b = img_matrix[,3],
  cluster = km$cluster
)
centroides <- img_df %>%
  group_by(cluster) %>%
  summarise_all(mean)


# Representação Hexadecial dos centroides

centroides <- centroides %>%
  mutate(cor = rgb(r, g, b))
centroides$cor

# Exibe a paleta de cores com base nos centroides

criar_paleta <- function(img, num_cores){
  # transforma a imagem em uma matriz
  img_matrix <- apply(img, 3, as.numeric)
  # treina o algoritmo de k médias
  km <- kmeans(img_matrix, centers = num_cores)
  img_df <- tibble(
    r = img_matrix[,1], 
    g = img_matrix[,2], 
    b = img_matrix[,3],
    cluster = km$cluster
  )
  # calcula os centroides dos grupos
  centroides <- img_df %>%
    group_by(cluster) %>%
    summarise_all(mean)
  # transforma a cor em hexadecimal
  centroides <- centroides %>%
    mutate(cor = rgb(r, g, b))
  # vetor de cores
  sort(centroides$cor)
}


exibir <- function(x) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  
  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
}
exibir(sort(centroides$cor))

#exporta arquivo de hexadecimais em csv
exporta_tabela = write.csv(centroides, "cor.csv")



