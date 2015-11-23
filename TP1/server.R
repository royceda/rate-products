
library(shiny);

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



shinyServer(function(input, output) {
  output$vie <- renderText({
    anu(input$cap, input$taux, input$maturite, input$periode);
    })

    output$vie1<- renderTable({
      ech(input$cap,input$taux,input$maturite,input$periode,input$fraisfixes);
      })

      output$vie2<- renderPlot({
        TEG = ech(input$cap,input$taux,input$maturite,input$periode,input$fraisfixes)$TEG;
        TEG = TEG[1:(length(TEG)-1)];
        plot(TEG);
        })
        })
