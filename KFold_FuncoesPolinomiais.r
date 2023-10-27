rm(list = ls())



#                                                                                   VALIDAÇÃO CRUZADA COM K GRUPOS (K = 10)   
#Verificando qual a função polinomial tem o menor EQM 


library(boot)
summary(Auto) #Base Trabalhada

set.seed(5) #Semente
Vetor_erro = rep(0, 10) #Cria um vetor nulo

for (i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto) #Utilizando laço de repetição para calcular funções de grau 1 até 10
  Vetor_erro[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1] }
  #calculando o erro quadrado médio dos dados de teste(cv.glm)

Vetor_erro
plot(Vetor_erro)

#Melhor função = polinomial de grau 6