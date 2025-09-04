##IBI5086-2025
##Profa. Júlia M Pavan Soler
##Exemplo de ANOVA (DCA) 1 fator em 3 níveis

install.packages("mixOmics")
library(mixOmics)
data('breast.TCGA')
names(breast.TCGA)
names(breast.TCGA$data.train)

# extrair dados (treinamento) de 3 BD
data = list(mRNA = breast.TCGA$data.train$mrna, 
            miRNA = breast.TCGA$data.train$mirna, 
            proteomics = breast.TCGA$data.train$protein)

lapply(data, dim)

# extrair dados de grupo
gr = breast.TCGA$data.train$subtype
summary(gr)

y1 <- as.data.frame(data$mRNA)
head(y1)
y2 <- as.data.frame(data$miRNA)
head(y2)
y3 <- as.data.frame(data$proteomics)
head(y3)

#Considere os dados da proteina y3[,1]
resp<-y3[,1]
resp
#An�lise descritiva inicial
tapply(resp,gr,mean)
tapply(resp,gr,sd)

boxplot(resp ~ gr, main="Boxplot de proteína.1 ")

#Neste caso: dados desbalanceados
table(gr)

#Alternativa: Gr�fico de m�dias com sd (adapte tamb�m para se)
library(ggplot2)
me<-tapply(resp,gr,mean)
sd<-tapply(resp,gr,sd)
dms<-data.frame(cbind(c("Basal","Herb2","LumA"),me,sd))
dms$me<-as.numeric(me)
dms$sd<-as.numeric(sd)
str(dms)
ggplot(dms, aes(x=V1, y=me)) + 
  geom_errorbar(aes(ymin=me-sd,ymax=me+sd,width=.1)) +
  geom_point() + 
  ggtitle("Gráfico de médias (sd)")

#Grafico com o perfil das m�dias
plot(me, type="l", main="Perfil de m�dias")
points(me, pch="x", col=2, cex=1.5)
#N�o � recomendado pois as m�dias n�o t�m esse padr�o "longitudinal"

#Ajuste do Modelo ANOVA-DCA
# Usando o comando aov
fit1 <- aov(y3[,1] ~ gr)
summary(fit1)
anova(fit1)
summary(fit1)[2,5]

par(mfrow=c(2,2))
plot(fit1)

plot(fit1$fit, fit1$res, xlab="Valores Ajustados", ylab="Res�duos")
title("Res�duos vs Preditos")

qqnorm(fit1$residuals,ylab="Residuos", main=NULL)
qqline(fit1$residuals)
title("Gr�fico Normal de Probabilidade dos Res�duos")

# Teste de normalidade de Shapiro-Wilk
# H0: distribui��o � normal
# p-valor > 0.05 n�o rejeita H0
shapiro.test(fit1$residuals)

#Teste da homogeneidade das vari�ncias dos grupos (homocedasticidade)
bartlett.test(resp,gr) 
#H0:Sigma_j=Sigma
#conclus�o?

fit1$coefficients
confint(fit1)

#interprete os coeficientes do modelo
#interprete descritivamente o valor F da ANOVA
#interprete as somas de quadrados (SQT=SQTrat+SQRes)
#QMRes estima qual par�metro?
#h� evid�ncia para efeito significante de Trat?
#Quais suposi��es devem estar satisfeitas para a validade da an�lise?

#Compara��es m�ltiplas de Tukey
plot(TukeyHSD(fit1))
TukeyHSD(fit1)

#Fun��o para calcular IC e pval para combina��es lineares espec�ficas das m�dias
ci=function(fitout,C,df,alpha=0.05) {   
  beta=fitout$coefficients
  V=vcov(fitout)
  Cb=C%*%beta
  se=round(sqrt(diag(C%*%V%*%t(C))),4)
  tobs=(C%*%beta)/se
  tval=qt(1-alpha/2,df)
  pval=2*pt(abs(tobs), df, lower.tail = FALSE)
  low=round(Cb-tval*se,4)
  up=round(Cb+tval*se,4)
  m=cbind("C",Cb,se,low,up,pval)
  dimnames(m)[[2]]=c(paste("C"),"estimate","se",paste(100*(1-alpha),"% Conf.",sep=""),"limits","pval")
  m 
} 

gl.1<-fit1$df.residual
C1=matrix(c(1/2,1/2,-1),nrow=1)
ci(fit1,C1,gl.1) #IC para o contraste: (mi.Basal+mi.Herb2)/2 - mi.LumA

C2=matrix(c(0,1,-1),nrow=1)
ci(fit1,C2,gl.1) #IC para a combina��o linear (mi.Herb2- mi.LumA) sem a corre��o de Tukey

C2=matrix(c(0,-1,1),nrow=1)
ci(fit1,C2,gl.1) #IC para a combina��o linear (mi.Herb2- mi.LumA) sem a corre��o de Tukey

#compare com TukeyHSD(fit1)

#Considere os dados da proteina y3[,i] para i=1:142
#Vamos avaliar o efeito de pelo menos um grupo
pvalue<-matrix(NA,142,1)
for (i in 1:142){
  resp[i]<-y3[,i]
  fit <- aov(y3[,i] ~ gr)
  pvalue[i]<-summary(fit)[[1]][1,5]
  i<-i+1
}
pvalue

plot(seq(1:142),-log10(pvalue), main="Efeito diferencial da Proteína")

#Correção para os múltiplos estes: Bonferroni
#0.05/142
min(pvalue)
max(-log10(pvalue))
which(-log10(pvalue)>38)

boxplot(y3[,51] ~ gr, main="Boxplot de proteína.51 ")


fit <- aov(y3[,51] ~ gr)
summary(fit)
anova(fit)
plot(TukeyHSD(fit))
TukeyHSD(fit)







