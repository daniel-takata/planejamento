boxplot(notas_criancas$nota~notas_criancas$grupo)

analise_notas=aov(nota~factor(grupo),data=notas_criancas)
anova(analise_notas)

library(lawstat)
levene.test(notas_criancas$nota,notas_criancas$grupo,
            location="mean")

pairwise.t.test(notas_criancas$nota,notas_criancas$grupo)
