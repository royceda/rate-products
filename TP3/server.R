library(shiny);

numextractall <- function(string){
  as.numeric(unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)), use.names=FALSE))
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


#####################################################
##################### TP3 ###########################
#############################################"#######"


#definition of ech
ech<-function(tx, coup, matu, cap){
    obli = list(tx, cap, matu, cap*coup)
    return(obli);
}

#cover portfolio
cover <- function(obli1, obli2, obli3){
    tmp = list(obli1, obli2, obli3);
    return(tmp);
}

#Fisher's formula
value <- function(obli){
    tx = obli[[1]];
    cap = obli[[2]];
    matu = obli[[3]];
    flux = obli[[4]];

    tmp = 0;

    for (i in (1:matu)){
        tmp = tmp + flux/(1+tx)^i;
    }
    tmp = tmp + cap/(1+tx)^matu;
    return(tmp);
}

#matrix b
values<- function(cov){
    tmp = list(value(cov[[1]]), value(cov[[2]]), value(cov[[3]]));
    vect = matrix(unlist(tmp), nrow=3);
    return(vect);
}



duration <- function(obli){
    tx = obli[[1]];
    cap = obli[[2]];
    matu = obli[[3]];
    flux = obli[[4]];

    tmp = 0;
      suiv = list();
    T = matu-1;

     for (i in (1:T)){
         a = flux*i/(1+tx)^(i);
        tmp = tmp + a;
         suiv = append(suiv, a);
    }

    a = cap*matu/(1+tx)^matu;
    tmp = tmp + a;
    suiv = append(suiv, a);
    res = tmp/value(obli);

    return(res);
}

sensibility <- function(obli){
    tx = obli[[1]];
    tmp = -duration(obli)/(1+tx);
    return(tmp);
}



#C = V"(r) / V(r)
#mesure le risque de taux
convexity <- function(obli){
     tx = obli[[1]];
    cap = obli[[2]];
    matu = obli[[3]];
    flux = obli[[4]];
    tmp = 0;

    suiv = list();
     for (i in (1:matu)){
        tmp = tmp + flux*i*(1+i)/((1+tx)^(i));
    }
    res = tmp/value(obli);
    return(res);
}




mat <- function(couv){
    n = length(cov);
    tmp = list();

    for(i in (1:n)){
        v = value(couv[[i]]);
        d = duration(couv[[i]])*v;
        c = convexity(couv[[i]])*v;
        tmp = append(tmp,list(v, d, c));
    }

    mat = matrix(unlist(tmp), ncol=3);
    return(mat);
}


lambda <- function(couv){
    A = mat(couv);
    b = values(couv);
    return(solve(A) %*% b);
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
