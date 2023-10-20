#                                   Validação Cruzada
#                Autonomia e potencia de autómóveis - comparando métodos

#Instalando pacotes
install.packages("ISLR2")
library(ISLR2)

#Gerando semente
set.seed(2)

#Base de dados
attach(Auto)
str(Auto)

#Separando dados de treino - amostra de 50% da base
train <- sample(392,196)

#Executando o modelo para dados de treino
#               Regressão Linear
modelo <- lm(mpg ~ horsepower, data = Auto, subset = train)

#Resumo de analise
modelo
summary(modelo)

#EQM com dados de Teste a partir do modelo linear:
#               mpg = -0.1513 * horsepower + 39.0280

mean((mpg - predict(modelo, Auto))[-train]^2)

# Comparando o erro com os valores na base pode-se avaliar que ele é alto: 25.72651

#-------------------------------------------------------------------------------------------

#EQM com dados de Teste a partir do modelo quadrático:
# usando o comando poly para modelos polinomais 
modelo2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(modelo2, Auto))[-train]^2)

# Comparando o erro com os valores na base pode-se avaliar que ele é médio: 20.43036

#-------------------------------------------------------------------------------------------

#EQM com dados de Teste a partir do modelo cúbicos:
# usando o comando poly para modelos polinomais 
modelo3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(modelo3, Auto))[-train]^2)

# Comparando o erro com os valores na base pode-se avaliar que ele é médio: 20.38533


