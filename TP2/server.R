
library(shiny);

##########################################################
################# TP1 ####################################
##########################################################


#Calcul l'annuités
anu <- function(cap, taux, maturite, periode)
{
  tmp = cap * taux
  anu = tmp / (1-(1+taux)^(-maturite))
  return(anu)
}

#Calcul de l'echeancier
ech <- function(cap, taux, maturite, periode, fraisfixes)
{
  res = c();
  CRD <- cap;
  anuit = anu(cap,taux,maturite,periode);
  taux = taux/periode;

  for (y in (1:maturite) ){
    i <- taux*CRD;
    ka <- anuit-i;
    txfr = i + fraisfixes;
    tauxreel = 100*periode*txfr/CRD;
    anureel = anuit + fraisfixes;
    CRD = CRD - ka;

    # cat ("date=",y," annuite=",anuit," annuite TEG=",anureel," interet=",i," amortissement=",ka," CRD = ",CRD, "taux reel = ",tauxreel,"\n")
    res = c(res, y, round(anuit,2), round(anureel,2), round(i,2), round(ka,2), round(CRD,2), round(tauxreel,3));
  }


  res = t(matrix(res,ncol=maturite));
  colnames(res) = c("date","annu","anu TEG","int","amor","CRD","TEG");
  res = data.frame(res);
  return(res);
}


##########################################################
################# TP2 ####################################
##########################################################

#Rembourssement anticipé
ra <- function(taux, CRD){
  tmp = taux * CRD;
  return(tmp);
}

#taux de refinnancement
ta <- function(CRD, penalties, flux){
  tmp = CRD*(1 + penalties)+flux;
  return(tmp);
}

# calcul le produit des (1+ri)
prod <- function(tab, n){
  tmp = 1;
  for (i in (1:n)){
    tmp = tmp * (1+ 0.01*tab[i]);
  }
  return(tmp);
}


#La collection des taux futurs la suite(ri^n)
futur <- function(tauxInit, tabTaux, size){
  tab <- c();
  tmp = tauxInit;
  tab = append(tab,tmp);

  for (i in (1:size)){
    tmp = (1 + tabTaux[i])**i / ((1 + tauxInit)* prod(tabTaux, size)) -1;
    tab = append(tab, tmp);
  }
  return(matrix(tab, nrow=1))
}


#tabtauxInit: liste des vrai taux initale
rates <- function(tabTaux, size){
  tab <- c();
  tmp <- c();
  n <- size;
  for (i in (1:n)){
    tmp = futur(tabTaux[i], tabTaux, n-i+1);
    if(i<n){
      for(k in (i:n)){
        tmp = append(tmp,0);
      }
    }
    tab = append(tab, tmp);
  }
  return(tab);
}


shinyServer(function(input, output) {
  output$vie <- renderText({
    anu(input$cap, input$taux, input$maturite, input$periode);
    })

  tabTaux <- c(0.01, 0.02, 0.03, 0.04, 0.01);
#tabTaux <- c(input$taux1, input$taux2, input$taux3, input$taux4, input$taux5);

    output$vie1<- renderTable({
      futur(input$taux, tabTaux, input$periode);
      })

      #output$vie2<- renderPlot({
        #TEG = ech(input$cap,input$taux,input$maturite,input$periode,input$fraisfixes)$TEG;
        #TEG = TEG[1:(length(TEG)-1)];
       # plot(TEG);
        #})
  })
