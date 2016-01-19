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

    Mat = c(value(couv[[1]]), duration(couv[[1]])*v, convexity(couv[[1]])*v) ;

    for(i in (2:n)){
        v = value(couv[[i]]);
        d = duration(couv[[i]])*v;
        c = convexity(couv[[i]])*v;
        Mat = rbind(Mat, c(v, d, c));
    }

   # mat = matrix(unlist(tmp), ncol=3);
    return(t(Mat));
}


lambda <- function(couv){
    A = mat(couv);
    b = values(couv);
    return(try(solve(A,b, tol=1e-58)));
}


#A modifier
shinyServer(function(input, output) {
  shinyServer(function(input, output) {
  output$vie <- renderText({
    anu(input$cap, input$taux1, input$maturite, input$periode);
    })

  #for test
  r = 0.03;
  coupon = 0.05;

  e1 = ech(r, coupon, 5, 100);
  e2 = ech(r, coupon, 3, 200);
  e3 = ech(r, coupon, 5, 150);
  e1;

  value(e1)

  cov = cover(e1, e2, e3);
  output$vie <- renderTable({ "couverture";});
    output$vie1<- renderTable({
      mat(cov);
      })
  })
})
