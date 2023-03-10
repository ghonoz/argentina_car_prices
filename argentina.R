# Importa??o das bibliotecas

library(tidyverse)
library(ggplot2)

# Importa??o da base de dados
df <- read.csv('argentina_cars_ajustado.csv', header = TRUE, sep = ',')
head(df)

df2 <- read.csv('argentina_cars_ajustado.csv', header = TRUE, sep = ',')
df2
ncolumn <- ncol(df)


# Verificando se n?o h? caracteres especiais
for (i in (1:ncolumn))
{
  
  print(unique(df[, i]))

}



# H? algumas linhas com caracteres especiais em 'body_type' que n?o foram exclu?das manipulando LibreOffice

df[df$body_type == 'Furgón',]

# Eliminar linhas 134, 164, 172:
df <- df[-c(134, 164, 172),]

# Criando nova vari?vel para, caso houver erro, n?o alterar a principal
df1 <- df

head(df1)

# Como h? dois tipos de moeda, irei deixar todas com o valor de d?lar, para n?o haver diferen?a nas an?lises
# Obs: h? alguns erros m?nimos de aproxima??o

for (i in 1:nrow(df1)) {
  df1[i, 1] <- 
    if (df1[i, 12] == 'pesos')
    {
      df1[i, 1] <- df[i, 1] * 0.0058
      
    } else {
      df1[i, 1] <- df1[i, 1]
      
    }
    
}
  
head(df1)
  
# Como a moeda j? foi ajustada para a mesma, n?o h? mais necessidade da coluna ''currrency''
df1$currency <- NULL

head(df1)
df1

p <- ggplot(df, aes(x = money)) 

p + geom_histogram(aes(y = ..density..), colour = 'white', fill = 'green') + 
  geom_density(alpha = .2)

shapiro.test(df$money)

# Teste de Shapiro Wilk resultou em um p-valor maenor que 0.05. Logo, n?o segue distribui??o normal.
# Por falta de conhecimentos atuais, n?o irei analisar isso, pois n?o conhe?o estat?stica n?o-param?trica


# Histograma da quilometragem: 
p <- ggplot(df, aes(x = kilometres)) + 
  geom_histogram(aes(y = ..density..), color = 'black', fill = 'orange') 

p

shapiro.test(df$kilometres)  

# Quilometragem tamb?m resultou em um valor muito pequeno, apesar do histograma parecer uma normal..

p <- ggplot(df, aes(x = kilometres, y = money)) 
p + geom_point() 

# O gr?fico n?o ficou dos melhores tamb?m.
# H? muitos valores de dinheiro baixos, mas n?o nulos, como 8000. Portanto, n?o podem ser retirados.


# Boxplot do valor do carro pelo tipo de combust?vel
p1 <- ggplot(df, aes(x = fuel_type, y = money, fill = fuel_type)) 
p1 + geom_boxplot() + 
  ggtitle('Valor do carro por tipo de combust?vel') + 
  xlab('Combust?vel') + ylab('Valor') + 
  labs(fill = 'Tipo de Combust?vel')

# Poss?vel perceber, sem muitas surpresas, que o carro h?brido ? maior no que tange ? pre?o.



# Transformar a coluna ano em fator para realizar gr?ficos com essa vari?vel:



