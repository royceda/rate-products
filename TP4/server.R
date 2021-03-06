library(shiny);
library(lpSolveAPI);


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
##################### TP4 ###########################
#############################################"#######"


#definition of ech
ech<-function(tx, coup, matu, cap){
    obli = list(tx, cap, matu, cap*coup)
    return(obli);
}

ech1<-function(tx, coup, matu, flx){
  obli = list(tx, cap, matu, cap*coup);
  return(obli);
}

#cover portfolio
#list of Bonds

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
#matrix b
values<- function(cov){
    n = length(cov);
    tmp = list();
    for (i in (1:n)){
        val = value(cov[[i]]);
        tmp = append(tmp, val);
    }
    vect = matrix(unlist(tmp), nrow=n);
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
    n = length(couv);
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

#Matrice de constrainte
matCon <- function(couv){
    n = length(cov);

    tmp = list();
    for(i in (1:n)){
        v = value(couv[[i]]);
        d = duration(couv[[i]])*v;
        c = convexity(couv[[i]])*v;
        tmp1 = list();

        for(j in (1:n)){
            if(i == j){
                tmp1 = append(tmp1, 1);
            }
            tmp1 = append(tmp1, 0);
        }

        tmp = append(tmp,list(v, d, c));
        tmp = append(tmp, tmp1);
    }

    mat = matrix(unlist(tmp), ncol=n);
    return(mat);
}



#Prepare le PL
LP <- function(cov){
    n = length(cov);

    tmp = list();
    for(i in (1:n)){
       tmp = append(tmp, value(cov[[i]]));
    }
    f.obj <- matrix(unlist(tmp), ncol=n);
    #f.obj <- c(1, 9, 1)


    f.con <- matCon(cov);
    #f.con <- matrix (c(1, 2, 3, 3, 2, 2), nrow=2, byrow=TRUE)

    tmp = list();
    for(i in (1:(n+1))){
        if(i <= 3){
            tmp = append(tmp, "=");
        }else{
            tmp = append(tmp, "<=");
        }
    }
    f.dir = matrix(unlist(tmp), nrow = (n+1));

    #f.dir <- c("=", "=", "=");
    f.rhs <- c(0, 0, 0);


    res = lp("min", f.obj, f.con, f.dir, f.rhs);
    return (res);
}



sol <- function(){
  passif = list("flux"=numextractall(input$flp),"dates"=numextractall(input$dp))
  tauxactif = numextractall(input$cbtp)
  matactif = numextractall(input$matp)
  cpriactif = c()

  for ( k in 1:length(tauxactif)){k
    obp=echinfine(tauxactif[k],matactif[k],100.)
    cpriactif=cbind(cpriactif,c(prixactu(obp,input$taux),sensibilite(obp,input$taux),convexite(obp,input$taux)))
  }

  cprixpassif=c(prixactu(passif,input$taux),sensibilite(passif,input$taux),convexite(passif,input$taux))
  print(cprixpassif)
  c_ = c(rep(0,length(tauxactif)))
  signs = c(rep('=', 3), rep('>=',length(tauxactif)))
  D=diag(length(tauxactif))

  res = lpSolve::lp('min',c_,rbind(cpriactif,D), signs, c(cprixpassif,rep(10,length(tauxactif))))
  print(res)
}






#A modifier

shinyServer(function(input, output) {
  r = input$vtaux;
  coupon = input$taux;


  #DATA
  r<- inpu$vtaux;
  coupon <- input$taux;
  flux <- numextractall(input$flp);
  dates <- numextractall(input$dp);
  taux <- numextractall(input$cbtp);
  matu <- numextractall(input$matp);

  n = length(fulx)
  cover = list();
  for(i in  1:n){
    cover = append(cover, ech1(taux[[i]], coupon, matu[[i]], flux[[i]]));
  }



  e1 = ech(r, coupon, 5, 100);
  e2 = ech(r, coupon, 3, 200);
  e3 = ech(r, coupon, 5, 150);
  e4 = ech(r, coupon, 5, 120);
  e5 = ech(r, coupon, 5, 110);
  e6 = ech(r, coupon, 5, 100);

  cov = list(e1,e2,e3,e4,e5,e6);



  output$vie <- renderTable({
    #anu(input$cap, input$taux1, input$maturite, input$periode);
    LP(cover);
    })

    real <- list(0.01, 0.02, 0.03, 0.04, 0.01);
    #read.table("filename", header = TRUE,  sep = "t",  stringsAsFactors = FALSE)l <- list(input$taux1, input$taux2, input$taux3, input$taux, input$taux );

    output$vie2 <- renderTable({sol()})
    })
