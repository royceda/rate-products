anu <- function(cap, taux, maturite, periode){
  tmp = cap * taux
  anu = tmp / (1-(1+taux)^(-maturite))
  return(anu)
}

#calcul les flux
actuf <- function(cap, taux, maturite, periode){
 tmp <- c();
 for (i in (1:maturite)){
  tmp = append(tmp, cap*taux);
 }
 return(tmp);
}

#Calcul Va d'un infine
#flux: tableau
#taux constant
#maturite constant
prixactu<-function(cap, flux, taux, maturite){
  tmp = 0;
  for(i in (1:maturite)){
    tmp = tmp + flux[i]/(1+taux)^i;
  }
  tmp = tmp + cap/(1+taux)^maturite
  return(tmp);
}

#Calcul la duration de l'actif
duration<-function(cap, flux, taux, maturite)
{
 tmp1 = 0;
 tmp2 = 0;

 for(i in (1:maturite)){
   tmp1 = tmp1 + flux[i]*t[i]/(1+taux)^t[i];
 }
 for(i in (1:maturite)){
   tmp2 = tmp2 + flux[i]/(1+taux)^t[i];
 }
 return(-tmp1/tmp2);
}


sensibilite<-function(cap, taux , flux, maturite)
{
  tmp = -duration(cap, flux, taux, maturite)/(1 + taux);
  return(tmp);
}


convexite<-function(cap, taux, flux, maturite){
  tmp1 = 0;
  tmp2 = 0;
  for(i in (1:maturite)){
    tmp1 = tmp1 + flux[i]*t[i]*(1+t[i])/(1+r)^(t[i]+2);
  }
  for(i in (1:maturite)){
    tmp2 = tmp2 + flux[i]/(1+r)^(t[i]);
  }
  return(tmp1/tmp2);
}


#derivé de la valeur actualisée en fonction du taux
variation <- function(cap, taux, flux, maturite){
  tmp = duration(cap,  taux, flux, maturite) * prixactu(cap, taux, flux, maturite);
  return(tmp;)
}


#Calcul de la matrice A
#n = 3 echeanchiers
#taux, flux, maturite et cap sont des tableaux
A <- function(cap, flux, taux, maturite, n){
  #ligne
  mat <- c();
  for(i in (1:n)){
    mat[1+i] = prixactu(cap[i], flux[i], taux[i], maturite[i]);
    mat[2+i] = duration(cap[i], flux[i], taux[i]) * prixactu(cap[i], flux[i], taux[i], maturite[i]);
    mat[3+i] = convexite(cap[i], flux[i], taux[i]) * prixactu(cap[i], flux[i], taux[i]);
  }
  return(matrix(mat, nrow=n, ncol=n));
}

#Comme précedement pour les donnees
B <- function(cap, flux, taux, maturite){
  mat <- c();

  for(i in (1:n)){
    mat[i] = prixactu(cap, flux, taux, maturite);
    mat[i] = duration(cap, flux, taux) * prixactu(cap, flux, taux, maturite);
    mat[i] = convexite(cap, flux, taux) * prixactu(cap, flux, taux);
  }
  return( matrix(mat, nrow=n, ncol=1));
}


#############################
#         TP4               #
#############################

#retourne la matrice des lambdas
#A et B seront construit au préalable
combi <- function(A, B){
  mat = solve(A,B);
  return(mat);
}


install.packages("lpSolve");
install.packages("lpSolveAPI");

library(lpSolveAPI);

#dans le cas PL
#P est la matrice collonne des prix
combi2 <- function(P, A, B, n){
  C <- c();
  for (i in (1:n)){
    C = append(C, "==");
  }

  lambda = lp("min", P, A, matrix(C, nrow=n, ncol=1 ), B);
  return(lambda);
}
