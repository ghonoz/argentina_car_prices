# Importação das bibliotecas

library(tidyverse)
library(ggplot2)

# Importação da base de dados
df <- read.csv('argentina_cars_ajustado.csv', header = TRUE, sep = ',')
head(df)

df2 <- read.csv('argentina_cars_ajustado.csv', header = TRUE, sep = ',')
df2
ncolumn <- ncol(df)


# Verificando se não há caracteres especiais
for (i in (1:ncolumn))
{
  
  print(unique(df[, i]))

}



# Há algumas linhas com caracteres especiais em 'body_type' que não foram excluídas manipulando LibreOffice

df[df$body_type == 'FurgÃ³n',]

# Eliminar linhas 134, 164, 172:
df <- df[-c(134, 164, 172),]

# Criando nova variável para, caso houver erro, não alterar a principal
df1 <- df

head(df1)

# Como há dois tipos de moeda, irei deixar todas com o valor de dólar, para não haver diferença nas análises
# Obs: há alguns erros mínimos de aproximação

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
  
# Como a moeda já foi ajustada para a mesma, não há mais necessidade da coluna ''currrency''
df1$currency <- NULL

head(df1)
df1

p <- ggplot(df, aes(x = money)) 

p + geom_histogram(aes(y = ..density..), colour = 'white', fill = 'green') + 
  geom_density(alpha = .2)

shapiro.test(df$money)

# Teste de Shapiro Wilk resultou em um p-valor maenor que 0.05. Logo, não segue distribuição normal.
# Por falta de conhecimentos atuais, não irei analisar isso, pois não conheço estatística não-paramétrica


# Histograma da quilometragem: 
p <- ggplot(df, aes(x = kilometres)) + 
  geom_histogram(aes(y = ..density..), color = 'black', fill = 'orange') 

p

shapiro.test(df$kilometres)  

# Quilometragem também resultou em um valor muito pequeno, apesar do histograma parecer uma normal..

p <- ggplot(df, aes(x = kilometres, y = money)) 
p + geom_point() 

# O gráfico não ficou dos melhores também.
# Há muitos valores de dinheiro baixos, mas não nulos, como 8000. Portanto, não podem ser retirados.


# Boxplot do valor do carro pelo tipo de combustível
p1 <- ggplot(df, aes(x = fuel_type, y = money, fill = fuel_type)) 
p1 + geom_boxplot() + 
  ggtitle('Valor do carro por tipo de combustível') + 
  xlab('Combustível') + ylab('Valor') + 
  labs(fill = 'Tipo de Combustível')

# Possível perceber, sem muitas surpresas, que o carro híbrido é maior no que tange à preço.



# Transformar a coluna ano em fator para realizar gráficos com essa variável:



