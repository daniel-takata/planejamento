boxplot(pilhas$tempo~pilhas$tipo)

pilhas_modelo=aov(tempo~factor(tipo),data=pilhas)
summary(pilhas_modelo)

c1=c(0.5,0.5,-0.5,-0.5)
c2=c(1,-1,0,0)
c3=c(0,0,1,-1)
contr=cbind(c1,c2,c3)

tipo_pilha_factor=factor(pilhas$tipo)
contrasts(tipo_pilha_factor)<-contr

pilhas_modelo_contr=aov(tempo~tipo_pilha_factor,data=pilhas)
summary.aov(pilhas_modelo_contr,split=list(tipo_pilha_factor=
            list("Contr. 1"=1, "Contr. 2"=2, "Contr. 3"=3)))

residuos=residuals(pilhas_modelo_contr)
residuos
hist(residuos)
qqnorm(residuos)
shapiro.test(residuos)
library(lawstat)
levene.test(pilhas$tempo,pilhas$tipo,location="mean")
