##################################################
# Dados microbiota pré e pós exposição de 3 meses ao antibiótico ATB1329A e placebo.
# P1 = Primeira análise populacional de microbiota
# P2 = Segunda análise populacional de microbiota (3 meses após a primeira analise)
# Ran 1 = Microbiotas de individuos submetidas ao uso de ATB1329A (Grupo Experimental)
# Ran 2 = Microbiotas de individuos submetidas ao uso de placebo (Grupo Controle)
##################################################

# Cenário 1 - Comparação Temporal - Geral


# Reprodutibilidade

set.seed(129) 

# --- Tamanho amostral ---

n <- 92 

# --- Gerar o P1 --- # Pré-tratamento

media1 <- 1000       # média arbitrária de P1
sd1 <- 150           # desvio padrão arbitrário de P1
P1 <- rnorm(n, mean = media1, sd = sd1)
P1


# --- Gerar o P2 --- # Pós-tratamento

b0 <- 100        # intercepto arbitrário
b1 <- 0.8         # coeficiente de P1 arbitrário
sd2 <- 80         # desvio do erro aleatório
P2 <- b0 + b1*P1 + rnorm(n, mean = 0, sd = sd2)
P2


# --- Gerar o Data frame ---
dados <- data.frame(P1, P2)
head(dados)
tail(dados)
str(dados)
names(dados)
summary(dados[,1:2])

# --- Realizar a estatística descritiva ---

library(psych)
describe(dados$P1)
describe(dados$P2)

# --- Avaliar a normalidade ---

shapiro.test(dados$P1)  # Teste de Shapiro-Wilk para normalidade P1
shapiro.test(dados$P2)  # Teste de Shapiro-Wilk para normalidade P2

#p> 0.05.

# --- RealizaR o teste paramétrico (t de Student pareado t) ---

t.test(dados$P2, dados$P1, paired = TRUE)

# --- Ajustar o modelo de regressão ---

modelo <- lm(P2 ~ P1, data = dados)

summary(modelo)


# --- Gerar o Boxplot ---

boxplot(P1, P2,
        names = c("P1 (Pré-tratamento)", "P2 (Pós-tratamento)"),
        col = c("lightblue", "lightpink"),
        main="Comparação Pré-tratamento x Pós-tratamento")


# --- Scatter plot (relação linear entre P1 e P2) ---

plot(dados$P1, dados$P2,
     xlab="Pré-tratamento",
     ylab="Pós-tratamento",
     main="Correlação Pré-tratamento x Pós-tratamento",
     pch=19, col="blue")
abline(lm(P2 ~ P1, data=dados), col="red", lwd=2) # linha de regressão


# Cenário 2 - Comparação de Grupos


# --- Reprodutibilidade ---

set.seed(129) 


# --- Tamanhos amostrais ---

n1 <- 35   # Ran=1
n2 <- 57   # Ran=2


# --- Gerar o Ran=1 ---
media_P1_1 <- 1000   # média de P1 para Ran=1
sd_P1_1 <- 150       # desvio padrão P1
P1_1 <- rnorm(n1, mean = media_P1_1, sd = sd_P1_1)
P1_1

media_P2_1 <- 700    # média de P2 para Ran=1 (abaixo de P1)
sd_P2_1 <- 80       # desvio padrão P2
P2_1 <- rnorm(n1, mean = media_P2_1, sd = sd_P2_1)
P2_1

# --- Gerar o Ran=2 ---
media_P1_2 <- 1005   # média de P1 para Ran=2
sd_P1_2 <- 150       # desvio padrão P1
P1_2 <- rnorm(n2, mean = media_P1_2, sd = sd_P1_2)
P1_2

media_P2_2 <- 1003    # média de P2 para Ran=2 (próximo de P1)
sd_P2_2 <- 100        # desvio padrão P2
P2_2 <- rnorm(n2, mean = media_P2_2, sd = sd_P2_2)
P2_2


# --- Gerar a variável Ran ---
Ran <- c(rep(1, n1), rep(2, n2))
Ran

# --- Concatenar os dados ---
P1 <- c(P1_1, P1_2)
P2 <- c(P2_1, P2_2)
P1
P2


# --- Gerar o Data frame final ---
dados2 <- data.frame(P1, P2, Ran)
head(dados2)
tail(dados2)
str(dados2)
names(dados2)
summary(dados2[,1:3])


# --- Realizar as estatísticas descritivas ---

library(psych)
describeBy(dados2$P1, dados2$Ran)
describeBy(dados2$P2, dados2$Ran)


# --- Realizar os teste de normalidade ---
shapiro.test(P1_1)
shapiro.test(P2_1)
shapiro.test(P1_2)
shapiro.test(P2_2)

# Verificar homocedasticidade
grupo_P1 <- factor(rep(c("P1_1","P1_2"), times=c(35,57)))
grupo_P2 <- factor(rep(c("P2_1","P2_2"), times=c(35,57)))

bartlett.test(P1 ~ grupo_P1)
bartlett.test(P2 ~ grupo_P2)

# --- Comparar os grupos ---
t.test(P1_1, P1_2)  # P1 Ran1 vs Ran2
t.test(P2_1, P2_2)  # P2 Ran1 vs Ran2

# --- Gerar os Boxplots ---
par(mfrow=c(1,2))
boxplot(P1 ~ Ran, data=dados2, col="lightblue", 
        main="Pré-tratamento por Grupo",
        names=c("Experimental", "Controle"))  


boxplot(P2 ~ Ran, data=dados2, col="lightpink", 
        main="Pós-tratamento por Grupo",
        names=c("Experimental", "Controle"))  


# --- Gerar o scatter plot ---
par(mfrow=c(1,1))
plot(dados2$P1, dados2$P2, col=Ran, pch=19,
     xlab="P1", ylab="P2", main="Relação Pré-tratamento x Pós-tratamento por Grupo")
legend("topleft", legend=c("Experimental", "Controle"), col=c(1,2), pch=19)


# Cenário 3 - Analise Bivariada (Comparação temporal dos grupos)


# --- Reprodutibilidade ---

set.seed(129)

library(MASS)   # Usar mvrnorm
library(psych)  # Fazer a estatísticas descritivas


# --- Tamanhos amostrais ---

n1 <- 35  # Ran=1 (Estudo)
n2 <- 57  # Ran=2 (Controle)


# --- Gerar Ran=1 ---

# Médias
media_P1_1 <- 1000
media_P2_1 <- 700
vmedia_1 <- c(media_P1_1, media_P2_1)

# Matriz de variâncias e covariâncias
sd_P1_1 <- 150
sd_P2_1 <- 80
cov_P1P2_1 <- 50
mcov_1 <- matrix(c(sd_P1_1^2, cov_P1P2_1,
                   cov_P1P2_1, sd_P2_1^2), nrow=2, byrow=TRUE)

# Dados bivariados
dados_1 <- mvrnorm(n1, mu=vmedia_1, Sigma=mcov_1)
P1_1 <- dados_1[,1]
P2_1 <- dados_1[,2]


# --- Gerar Ran=2 ---

# Médias
media_P1_2 <- 1005
media_P2_2 <- 1000
vmedia_2 <- c(media_P1_2, media_P2_2)

# Matriz de variâncias e covariâncias
sd_P1_2 <- 150
sd_P2_2 <- 100
cov_P1P2_2 <- 60
mcov_2 <- matrix(c(sd_P1_2^2, cov_P1P2_2,
                   cov_P1P2_2, sd_P2_2^2), nrow=2, byrow=TRUE)

# Dados bivariados
dados_2 <- mvrnorm(n2, mu=vmedia_2, Sigma=mcov_2)
P1_2 <- dados_2[,1]
P2_2 <- dados_2[,2]


# --- Gerar os dados concatenados ---

P1 <- c(P1_1, P1_2)
P2 <- c(P2_1, P2_2)
Ran <- c(rep(1, n1), rep(2, n2))

dados3 <- data.frame(P1, P2, Ran)
head(dados3)
tail(dados3)
str(dados3)
names(dados3)
summary(dados3[,1:3])


# --- Gerar os boxplots ---

boxplot(P1 ~ Ran, data=dados3, col="lightblue", main="Pré-tratamento por Grupo",
        names = c("Experimental", "Controle"))

boxplot(P2 ~ Ran, data=dados3, col="lightpink", main="Pós-tratamento por Grupo",
        names = c("Experimental", "Controle"))


# --- Realizar as estatísticas descritivas ---

describeBy(dados3$P1, dados3$Ran)
describeBy(dados3$P2, dados3$Ran)


# --- Realizar o teste de normalidade ---

shapiro.test(P1_1)
shapiro.test(P2_1)
shapiro.test(P1_2)
shapiro.test(P2_2)


# --- Comparar os grupos ---

t.test(P1_1, P1_2)  # P1 Ran1 vs Ran2
t.test(P2_1, P2_2)  # P2 Ran1 vs Ran2


# --- Gerar o scatter plot ---

plot(dados3$P1, dados3$P2, col=Ran, pch=19,
     xlab="P1", ylab="P2", main="Relação Pré-tratamento x Pós-tratamento por Grupo")
legend("topleft", legend=c("Experimental", "Controle"), col=c(1,2), pch=19)


