options(scipen=999)
## Ingrese los valores entregados por CADEM
n=702 ## nro de encuestados
llamados=7073 ## nro de llamadas hasta lograr 704 seleccionados que contestan la encuesta
elegibles=13314848 ## nro de elegibles declarados en CADEM

pc1.s1=round(n/llamados,3);pc1.s1*100 ## tasa de respuesta
pc0.s1=1-pc1.s1;pc0.s1*100 ## tasa de respuesta
ps1.e1=llamados/elegibles;ps1.e1*100 ## prob. de ser seleccionado para contestar encuesta
ps0.e1=1-ps1.e1;ps0.e1*100
p.elegible=n/elegibles;p.elegible*100
(1-p.elegible)*100
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

#################################
### APROBACIÓN PRESIDENCIAL #####
#################################
a=round(n*0.34);a #nro personas aprueba
b=round(n*0.59);b #nro personas desaprueba
c=round(n*0.03);c #nro personas ni aprueba ni desaprueba
d=round(n*0.04);d #nro personas no sabe no responde

## NIVEL 1: OPINIÓN DE LOS ENCUESTADOS
data.frame(
  `Opinión`=c("Aprueba",
            "Desaprueba",
            "No aprueba ni desaprueba",
            "No contesta o no sabe"),
  Prop=c(opinion(a),opinion(b),opinion(c),opinion(d))
)

## NIVEL 2: EXTRAPOLACIÓN A OPINIÓN SELECCIONADOS
data.frame(
  `Opinión`=c("Aprueba",
              "Desaprueba",
              "No aprueba ni desaprueba",
              "No contesta o no sabe"),
  cota.inf=c(ext.sel(a)[1]*100,ext.sel(b)[1]*100,
             ext.sel(c)[1]*100,ext.sel(d)[1]*100),
  cota.sup=c(ext.sel(a)[2]*100,ext.sel(b)[2]*100,
             ext.sel(c)[2]*100,ext.sel(d)[2]*100)
)

## NIVEL 3: EXTRAPOLACIÓN A OPINIÓN ELEGIBLES
data.frame(
  `Opinión`=c("Aprueba",
              "Desaprueba",
              "No aprueba ni desaprueba",
              "No contesta o no sabe"),
  cota.inf=c(ext.eleg(a)[1]*100,ext.eleg(b)[1]*100,
             ext.eleg(c)[1]*100,ext.eleg(d)[1]*100),
  cota.sup=c(ext.eleg(a)[2]*100,ext.eleg(b)[2]*100,
             ext.eleg(c)[2]*100,ext.eleg(d)[2]*100)
)


#########################################
### PERSPECTIVA PLEBISCITO DE SALIDA #####
#########################################
a=round(n*0.51);a #nro personas rechaza convención
b=round(n*0.33);b #nro personas aprueba convención
c=round(n*0.16);c #nro personas no sabe o no responde

## NIVEL 1: OPINIÓN DE LOS ENCUESTADOS
data.frame(
  `Opinión`=c("Rechaza",
              "Aprueba",
              "No contesta o no sabe"),
  Prop=c(opinion(a),opinion(b),opinion(c))
)

## NIVEL 2: EXTRAPOLACIÓN A OPINIÓN SELECCIONADOS
data.frame(
  `Opinión`=c("Rechaza",
              "Aprueba",
              "No contesta o no sabe"),
  cota.inf=c(ext.sel(a)[1]*100,ext.sel(b)[1]*100,
             ext.sel(c)[1]*100),
  cota.sup=c(ext.sel(a)[2]*100,ext.sel(b)[2]*100,
             ext.sel(c)[2]*100)
)

## NIVEL 3: EXTRAPOLACIÓN A OPINIÓN ELEGIBLES
data.frame(
  `Opinión`=c("Rechaza",
              "Aprueba",
              "No contesta o no sabe"),
  cota.inf=c(ext.eleg(a)[1]*100,ext.eleg(b)[1]*100,
             ext.eleg(c)[1]*100),
  cota.sup=c(ext.eleg(a)[2]*100,ext.eleg(b)[2]*100,
             ext.eleg(c)[2]*100)
)

