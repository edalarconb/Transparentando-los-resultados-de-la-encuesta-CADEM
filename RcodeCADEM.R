library(tidyr)
library(animation)
library(gganimate)

options(scipen=999)

elegibles=13314848 ## nro de elegibles declarados en CADEM

###############################################
###############################################
###############################################
## CAMBIAR SEMANA A SEMANA ####################
###############################################

n=701 ## nro de encuestados
llamados=7193 ## nro de llamadas realizadas

### PERSPECTIVA PLEBISCITO DE SALIDA #####
##### CAMBIAR PROPORCIONES ###############
a=round(n*0.39);a #nro personas aprueba convención
b=round(n*0.47);b #nro personas rechaza convención
c=round(n*0.14);c #nro personas no sabe o no responde
#############################################

pc1.s1=round(n/llamados,3);pc1.s1*100 ## tasa de respuesta
pc0.s1=1-pc1.s1;pc0.s1*100 ## tasa de no respuesta
ps1.e1=llamados/elegibles;ps1.e1*100 ## prob. de ser seleccionado para contestar encuesta
ps0.e1=1-ps1.e1;ps0.e1*100
p.elegible=n/elegibles;p.elegible*100


## NIVEL 1: OPINIÓN DE LOS ENCUESTADOS
opinion=function(x){
  op=round(x/n,2)
  return(op)
}

## NIVEL 2: EXTRAPOLACIÓN A SELECCIONADOS
ext.sel=function(x){
  lb=opinion(x)*pc1.s1
  ub=opinion(x)*pc1.s1+pc0.s1
  return(c(cota.inf=lb,cota.sup=ub))
}

## NIVEL 2: EXTRAPOLACIÓN A ELEGIBLES
ext.eleg=function(x){
  lb=opinion(x)*pc1.s1*ps1.e1
  ub=(opinion(x)*pc1.s1+pc0.s1)*ps1.e1+ps0.e1
  return(c(cota.inf=lb,cota.sup=ub))
}


## NIVEL 1: OPINIÓN DE LOS ENCUESTADOS
data.frame(
  `Opinión`=c("Aprueba",
              "Rechaza",
              "No contesta o no sabe"),
  Prop=c(opinion(a),opinion(b),opinion(c))
)

## NIVEL 2: EXTRAPOLACIÓN A OPINIÓN SELECCIONADOS
data.frame(
  `Opinión`=c("Aprueba",
              "Rechaza",
              "No contesta o no sabe"),
  cota.inf=c(ext.sel(a)[1]*100,ext.sel(b)[1]*100,
             ext.sel(c)[1]*100),
  cota.sup=c(ext.sel(a)[2]*100,ext.sel(b)[2]*100,
             ext.sel(c)[2]*100)
)

## NIVEL 3: EXTRAPOLACIÓN A OPINIÓN ELEGIBLES
data.frame(
  `Opinión`=c("Aprueba",
              "Rechaza",
              "No contesta o no sabe"),
  cota.inf=c(ext.eleg(a)[1]*100,ext.eleg(b)[1]*100,
             ext.eleg(c)[1]*100),
  cota.sup=c(ext.eleg(a)[2]*100,ext.eleg(b)[2]*100,
             ext.eleg(c)[2]*100)
)

#### gráfica de lo reportado
d1.1=data.frame(
  `Opinión`=c("Aprueba",
              "Rechaza",
              "NS/NR"),
  Prop=c(opinion(a)*100,
         opinion(b)*100,
         opinion(c)*100),
  nper=c(a,b,c)
)

colors <- c("cornflowerblue", "orange3", "turquoise3")
d1.1 %>%
  ggplot(aes(x = 1, y = Prop, fill = `Opinión`)) +
  geom_col(width = 1)+
  scale_fill_manual(values = colors,
                    name = "Opción")+
  coord_polar(theta = "y", start = 1,
              direction=1)+
  geom_text(aes(label = paste0(Prop,"% ",`Opinión`," (",nper,")"), 
                x = 1.2),
            position = position_stack(vjust = 0.5),
            size=4) +
  theme_void() +
  theme(legend.position="none")



### Animación extrapolación a los seleccionados
l=40
escenarios1=tibble(ddd=rep(seq(1,l),3),
                   resp=c(
                     rep("Aprueba", l),
                     rep("Rechaza",l),
                     rep("NS/NR",l)),
                   porc=round(c(seq(opinion(a)*pc1.s1*100,
                                    (opinion(a)*pc1.s1+pc0.s1)*100,
                                    length=l),
                                seq((opinion(b)*pc1.s1+pc0.s1)*100,
                                    opinion(b)*pc1.s1*100,length=l),
                                rep(opinion(c)*pc1.s1*100,l)),3))

escenarios1$resp=factor(escenarios1$resp,
                        levels=c("Aprueba","Rechaza","NS/NR"))

escenarios2=tibble(ddd=rep(seq(1,l),3),
                   resp=c(
                     rep("Aprueba", l),
                     rep("Rechaza",l),
                     rep("NS/NR",l)),
                   porc=round(c(
                     rep(opinion(a)*pc1.s1*100,length=l),
                     seq((opinion(b)*pc1.s1+pc0.s1)*100,
                               opinion(b)*pc1.s1*100,length=l),
                     seq(opinion(c)*pc1.s1*100,
                               (opinion(c)*pc1.s1+pc0.s1)*100,length=l)),3))

escenarios2$resp=factor(escenarios2$resp,
                        levels=c("Rechaza","NS/NR","Aprueba"))


escenarios3=tibble(ddd=rep(seq(1,l),3),
                   resp=c(
                     rep("Aprueba", l),
                     rep("Rechaza",l),
                     rep("NS/NR",l)),
                   porc=round(c(
                     seq((opinion(a)*pc1.s1+pc0.s1)*100,
                         opinion(a)*pc1.s1*100,length=l),
                     rep(opinion(b)*pc1.s1*100,length=l),
                     seq(opinion(c)*pc1.s1*100,
                         (opinion(c)*pc1.s1+pc0.s1)*100,length=l)),3))

escenarios3$resp=factor(escenarios3$resp,
                        levels=c("Aprueba","NS/NR","Rechaza"))


colors1 <- c("cornflowerblue", "orange3", "turquoise3")
colors2 <- c("orange3", "turquoise3", "cornflowerblue") 
colors3 <- c("cornflowerblue", "turquoise3","orange3")


gg1 <- escenarios1 %>%
  ggplot(aes(x = 1, y = porc, fill = resp)) +
  geom_col(width = 1)+
  scale_fill_manual(values = colors1,
                    name = "Opción")+
  coord_polar(theta = "y", start = 1,
              direction=1)+
  geom_text(aes(label = paste(porc,"% ",resp), 
                x = 1.2),
            position = position_stack(vjust = 0.5),
            size=6) +
  theme_void() +
  theme(legend.position="none")+
  transition_time(ddd)
animate(gg1, fps=2)


gg2 <- escenarios2 %>%
  ggplot(aes(x = 1, y = porc, fill = resp)) +
  geom_col(width = 1)+
  scale_fill_manual(values = colors2,
                    name = "Opción")+
  coord_polar(theta = "y", start = 1,
              direction=1)+
  geom_text(aes(label = paste(porc,"% ",resp), 
                x = 1.3),
            position = position_stack(vjust = 0.5),
            size=6) +
  theme_void() +
  theme(legend.position="none")+
  transition_time(ddd)
animate(gg2, fps=2)

gg3 <- escenarios3 %>%
  ggplot(aes(x = 1, y = porc, fill = resp)) +
  geom_col(width = 1)+
  scale_fill_manual(values = colors3,
                    name = "Opción")+
  coord_polar(theta = "y", start = 1,
              direction=1)+
  geom_text(aes(label = paste(porc,"% ",resp), 
                x = 1.3),
            position = position_stack(vjust = 0.5),
            size=6) +
  theme_void() +
  theme(legend.position="none")+
  transition_time(ddd)
animate(gg3, fps=2)
