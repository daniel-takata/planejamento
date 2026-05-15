boxplot(soja$Producao~soja$Tratamento)
boxplot(soja$Producao~soja$Linha)
boxplot(soja$Producao~soja$Coluna)

anova_soja=aov(Producao~factor(Linha)+factor(Coluna)+
                 factor(Tratamento),data=soja)
summary(anova_soja)

2*pt(-14.3,37)

residuos=residuals(anova_soja)
hist(residuos)
qqnorm(residuos)
shapiro.test(residuos)

raiz_producao1=1/sqrt(soja$Producao)
anova_soja=aov(raiz_producao1~factor(Linha)+factor(Coluna)+
                 factor(Tratamento),data=soja)
summary(anova_soja)

residuos=residuals(anova_soja)
residuos
qqnorm(residuos)
shapiro.test(residuos)

library(lawstat)
levene.test(residuos,soja$Tratamento,location="mean")


modelo=lm(raiz_producao1~factor(Linha)+factor(Coluna)+
                 factor(Tratamento),data=soja)
summary(modelo)
residuals(modelo)


anova_baterias=aov(duracao~factor(modelo)+factor(temperatura),
                   data=baterias_blocos_incompl)
summary(anova_baterias)

anova_baterias1=aov(duracao~factor(temperatura)+factor(modelo),
                   data=baterias_blocos_incompl)
summary(anova_baterias1)
