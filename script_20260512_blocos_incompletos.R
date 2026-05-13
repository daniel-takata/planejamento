#quadrados greco latinos

library(magic)
install.packages("magic")
rlatin(5)
install.packages("agricolae")
library(agricolae)
tratamentos=LETTERS[1:5]
tratamentos
letras_gregas=1:5
QGL=design.graeco(trt1=tratamentos,trt2=letras_gregas)
QGL

boxplot(foguetes$Resposta~foguetes$Formulacao)
boxplot(foguetes$Resposta~foguetes$Lote)
boxplot(foguetes$Resposta~foguetes$Operador)
boxplot(foguetes$Resposta~foguetes$Conjunto)

foguetes_anova=aov(Resposta~factor(Conjunto)+factor(Lote)+factor(Formulacao)+factor(Operador)
                     ,data=foguetes)
summary(foguetes_anova)


#experimentos em bloco incompletos balanceados


#ordem incorreta

baterias_anova=aov(duracao~factor(modelo)+factor(temperatura),
                   data=baterias_blocos_incompl)
summary(baterias_anova)

#ordem correta

baterias_anova1=aov(duracao~factor(temperatura)+factor(modelo),
                   data=baterias_blocos_incompl)
summary(baterias_anova1)
