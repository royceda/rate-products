library(shiny);

numextractall <- function(string){
  as.numeric(unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)), use.names=FALSE))
}

#calcul des annuités
anu <- function(cap, taux, maturite, periode)
{
  tmp = cap * taux
  anu = tmp / (1-(1+taux)^(-maturite))
  return(anu)
}

#flux ????
actuf <- function(cap, taux, maturite, periode)
{
  return(cap*taux);
}

#On regroupe dans une liste
#1 taux
#2 cap
#3 matu
#4 flux: supposés constant
echinfine<-function(tx, matu, cap)
{
  #on suppose des flux constants
  flux = cap*tx;
  obli = c(tx, cap, matu, flux);
  return(obli);
}

#Formule de Irving Fisher
prixactu<-function(obli, taux){
  tx   = obli[1];
  cap  = obli[2];
  matu = obli[3];
  flux = obli[4];
  tmp  = 0;

  for(i in (1:matu)){
    tmp = tmp + flux/(1 + taux)^i;
  }
  tmp = tmp + cap/(1 + taux)^matu
  return(tmp);
}


# D = V'(r) / V(r);
#modif: (obli, taux)
duration<-function(obli, taux){
  tx   = obli[1];
  cap  = obli[2];
  matu = obli[3];
  flux = obli[4];

  tmp1 = 0;
  tmp2 = 0;

  for(i in (1:matu)){
    tmp1 = tmp1 + flux*i/(1+taux)^i;
  }

  for(i in (1:matu)){
    tmp2 = tmp2 + flux/(1+taux)^i;
  }

  return(-tmp1/tmp2);
}


#Avec duration
#S = - D/(1+r)
sensibilite<-function(obli,taux)
{
  tmp = -duration(obli, taux)/(1 + taux);
  return(tmp);
}


sensibiliter<-function(obli, taux)
{

}



#C = V"(r) / V(r)
#mesure le risque de taux
convexite<-function(obli, taux){
  tx   = obli[1];
  cap  = obli[2];
  matu = obli[3];
  flux = obli[4];

  tmp1 = 0;
  tmp2 = 0;

  for(i in (1:matu)){
    tmp1 = tmp1 + flux*i*(1+i)/(1+taux)^(i+2);
  }
  for(i in (1:matu)){
    tmp2 = tmp2 + flux/(1+taux)^i;
  }

  return(tmp1/tmp2);
}



convexiter<-function(obl, taux){

}


ech <- function(cap,taux,maturite,periode,fraisfixes)
{
  res = c()
  CRD <- cap;
  anuit = anu(cap, taux, maturite, periode);
  taux = taux/periode;
  for (y in (1:maturite) ){
    i <- taux*CRD;
    ka <- anuit-i;
    txfr = i + fraisfixes * cap/periode;
    tauxreel = 100*periode*txfr/CRD;
    anureel = anuit + fraisfixes * cap/periode;
    CRD = CRD - ka;

    # cat ("date=",y," annuite=",anuit," annuite TEG=",anureel," interet=",i," amortissement=",ka," CRD = ",CRD, "taux reel = ",tauxreel,"\n")
    res = c(res,as.integer(y),round(anuit,2),round(anureel,2),round(i,2),round(ka,2),round(CRD,2),round(tauxreel,3));

  }
 res = t(matrix(res,ncol=maturite));
 colnames(res) = c("date","annu","anuTEG","int","amor","CRD","TEG");
 res = data.frame(res);
return(res)
}


shinyServer(function(input, output) {
   output$vie<- renderTable({
     passif    = list("flux"=numextractall(input$flp),"dates"=numextractall(input$dp))
     tauxactif = numextractall(input$cbtp)
     matactif  = numextractall(input$matp)
     cpriactif = c()

     for ( k in 1:length(tauxactif)){k
         obp       = echinfine(tauxactif[k], matactif[k], 100.)
         cpriactif = cbind(cpriactif,c(prixactu(obp, input$taux), sensibilite(obp, input$taux), convexite(obp, input$taux)))
     }
     cprixpassif = c(prixactu(passif, input$taux), sensibilite(passif, input$taux), convexite(passif, input$taux))

     #Linear Program
     print(cprixpassif);
     c_    = c(rep(0,length(tauxactif))); #Matrice economique
     signs = c(rep('=', 3), rep('>=', length(tauxactif))); #signe des contraintes
     D     = diag(length(tauxactif)); #matrice des contraintes
     res   = lpSolve::lp('min', c_, rbind(cpriactif, D), signs, c(cprixpassif, rep(10, length(tauxactif)))) #matrice des lambdas
     print(res);

     #calcul d'actif
     cpriactif1 = c()
     for ( k in 1:length(tauxactif)){k
       obp = echinfine(tauxactif[k],matactif[k],100.)
       cpriactif1 =   cbind(cpriactif1,c(prixactu(obp, input$taux+input$vtaux), sensibilite(obp,input$taux+input$vtaux), convexite(obp, input$taux+input$vtaux)))
     }
     vecte = cpriactif1%*%res$solution

      F = rbind(c(cpriactif%*%res$solution, rep("", length(tauxactif)-3)), res$solution, c(prixactu(passif,input$taux+input$vtaux)-vecte[1], rep("", length(tauxactif)-1)));
      rownames(F) <- c("passif"," Lambda", "ecart");

     return(F);
#     vectb=prixactu(obp,input$taux+input$vtaux)-matrix(c(prixactu(oba1,input$taux+input$vtaux),prixactu(oba2,input$taux+input$vtaux)),ncol=2)%*%lam

#     ecart=c(vecte,vectb,0)
#     A=cbind(vect0,vect1,vect2,vect3,resc,res,-ecart)
  })

})
