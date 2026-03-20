ybarra_A=mean(analgesicos[1:17,2])
ybarra_B=mean(analgesicos[18:35,2])
ybarra_A
ybarra_B

s2_A=var(analgesicos[1:17,2])
s2_B=var(analgesicos[18:35,2])
s2_A
s2_B

boxplot(analgesicos[,2]~analgesicos[,1])

install.packages("lawstat")
library(lawstat)

levene.test(analgesicos[,2],analgesicos[,1],location="mean")

sp_2=(16*s2_A+17*s2_B)/33
sp_2

t_obs=(ybarra_A-ybarra_B)/sqrt(sp_2*(1/17+1/18))
t_obs

pvalor=pt(-abs(t_obs),33)+(1-pt(abs(t_obs),33))
2*pt(-abs(t_obs),33)
pvalor                           

t.test(analgesicos[,2]~analgesicos[,1],var.equal=TRUE)

z=matrix(0,35,1)
z[1:17]=abs(analgesicos[1:17,2]-ybarra_A)
z[18:35]=abs(analgesicos[18:35,2]-ybarra_B)

summary(modelo)



t.test(z~analgesicos[,1],var.equal=TRUE)
levene.test(analgesicos[,2],analgesicos[,1],location="mean")



modelo=lm(analgesicos[,2]~factor(analgesicos[,1]))
anova(modelo)          
