set.seed(153)

# Irei usar o mesmo desvio padrão para gerar os datasets para garantir homocedasticidade no exercício
# Gerar 4 datasets independentemente
# rnorm() para garantir distribuição normal em todos os conjuntos

var <- 20

n1_1 <- rnorm(35, mean = 100, sd = var)
n2_1 <- rnorm(35, mean = 100, sd = var)
n1_2 <- rnorm(57, mean = 98, sd = var)
n2_2 <- rnorm(57, mean = 130, sd = var)

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

# Teste de homocedasticidade dos devios padrões
P1 <- c(n1_1, n1_2)
P2 <- c(n2_1, n2_2)

grupo_P1 <- factor(rep(c("P1_1","P1_2"), times=c(35,57)))
grupo_P2 <- factor(rep(c("P2_1","P2_2"), times=c(35,57)))

bartlett.test(P1 ~ grupo_P1)
bartlett.test(P2 ~ grupo_P2)

# Análise descritiva dos datasets
summary(n1_1)
summary(n1_2)
summary(n2_1)
summary(n2_2)

# Teste t
t.test(n1_1, n1_2)  # P1 Ran1 vs Ran2
t.test(n2_1, n2_2)  # P2 Ran1 vs Ran2

# Plots
par(mfrow=c(1,2))
boxplot(n1_1, n1_2, main="P1", names=c("Controle", "Correu"))
boxplot(n2_1, n2_2, main="P2", names=c("Controle", "Correu"))

