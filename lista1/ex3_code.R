set.seed(153)

# Irei usar o mesmo desvio padrão para gerar os datasets para garantir homocedasticidade no exercício
# Gerar 4 datasets independentemente
# rnorm() para garantir distribuição normal em todos os conjuntos

var <- 5

# Gerar médias aleatórias entre [30,60] para serem as médias dos conjuntos
medias <- runif(4, min = 30, max = 60) 

n1_1 <- rnorm(35, mean = medias[1], sd = var)
n2_1 <- rnorm(35, mean = medias[2], sd = var)
n1_2 <- rnorm(57, mean = medias[3], sd = var)
n2_2 <- rnorm(57, mean = medias[4], sd = var)

# Plot histogramas
par(mfrow=c(2,2))
hist(n1_1)
hist(n2_1)
hist(n1_2)
hist(n2_2)

# Teste de normalidade dos conjuntos
shapiro.test(n1_1)
shapiro.test(n1_2)
shapiro.test(n2_1)
shapiro.test(n2_2)

# Análise descritiva dos datasets
summary(n1_1)
summary(n1_2)
summary(n2_1)
summary(n2_2)

#
par(mfrow=c(1,1))
plot(n1_2~n1_1)
