set.seed(120)

var <- 15

n1_1 <- rnorm(200, mean = 100, sd = var)
n2_1 <- rnorm(200, mean = 100, sd = var)
n1_2 <- rnorm(200, mean = 98, sd = var)
n2_2 <- rnorm(200, mean = 130, sd = var)

#Simulação de dados
#Exemplo 1: Cenário 3 Lista01
#Vamos considerar os dados do grupo Ran=1
#matriz de correlação dos dados P1 e P2 para Ran=1

# Vetor de médias
mean_g1 <- c(mean(n1_1),mean(n2_1))
mean_g2 <- c(mean(n1_2),mean(n2_2))

# Calcular o vetor de correlação
cor_g1 <- cor(cbind(n1_1,n2_1)) # Calcula matriz de correlação
cor_g2 <- cor(cbind(n1_2,n2_2)) # Calcula matriz de correlação

#matriz de covariância dos dados P1 e P2 para Ran=1
cov_g1 <- cov(cbind(n1_1,n2_1)) # Calcula matriz de covariância
cov_g2 <- cov(cbind(n1_2,n2_2))

#Gerando dados da Normal bivariada
require(MASS)
n <-200

data_g1 <- mvrnorm(n,mean_g1,cov_g1)
data_g2 <- mvrnorm(n,mean_g2,cov_g2)

# Perguntei pro gpt uma forma de visualizar estes dados
# Caso os pontos se alinhassem na diagonal existe forte correlação das variáveis
# Caso só formassem uma nuvem, provavelmente há uma correlação próxima a zero

plot(data_g1[,1], data_g1[,2],
     xlab = "P1 (simulado)",
     ylab = "P2 (simulado)",
     main = "Scatterplot - Grupo Ran=1",
     pch = 19, col = "blue")

plot(data_g2[,1], data_g2[,2],
     xlab = "P1 (simulado)",
     ylab = "P2 (simulado)",
     main = "Scatterplot - Grupo Ran=2",
     pch = 19, col = "blue")

df <- data.frame(rbind(data_g1,data_g2))
Ransim<-c(rep(1,200),rep(2,200))
df <- cbind(df,Ransim)
colnames(df) <- c("P1","P2","Ransim")

fit <- manova(cbind(P1, P2) ~ Ransim, data = df)
summary(fit, test = "Pillai")

t.test(P1 ~Ransim, data = df)
t.test(P2 ~Ransim, data = df)
