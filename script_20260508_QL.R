install.packages("magic")
library(magic)
rlatin(4)
rlatin(7)

boxplot(purificadores$y~purificadores$B)
boxplot(purificadores$y~purificadores$M)
boxplot(purificadores$y~purificadores$H)

anova_purificadores=aov(y~factor(M)+factor(H)+factor(B),
                        data=purificadores)
summary(anova_purificadores)

modelo=lm(y~factor(M)+factor(H)+factor(B),
                        data=purificadores)
summary(modelo)
anova(modelo)
residuos=residuals(anova_purificadores)

library(lawstat)
levene.test(residuos,purificadores$B,location="mean")
qqnorm(residuos)
shapiro.test(residuos)
