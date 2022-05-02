options(scipen=999)
## Ingrese los valores entregados por CADEM
n=703 ## nro de encuestados
llamados=4983 ## nro de llamadas hasta lograr 704 seleccionados que contestan la encuesta
elegibles=13314848 ## nro de elegibles declarados en CADEM

pc1.s1=round(n/llamados,3) ## tasa de respuesta
pc0.s1=1-pc1.s1 ## tasa de respuesta
ps1.e1=llamados/elegibles ## prob. de ser seleccionado para contestar encuesta
ps0.e1=1-ps1.e1

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
a=round(n*0.35);a #nro personas que elige opción a
b=round(n*0.53);b #nro personas que elige opción b
c=round(n*0.06);c #nro personas que elige opción c
d=round(n*0.06);d #nro personas que elige opción d
a+b+c+d
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
a=round(n*0.46);a #nro personas que elige opción a
b=round(n*0.36);b #nro personas que elige opción b
c=round(n*0.18);c #nro personas que elige opción c

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

