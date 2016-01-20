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
##################### TP5 ###########################
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




#A modifier

shinyServer(function(input, output) {

  x = input$x;
  y = input$y;


  x = c(1,2,3,4,5,7,8)
  y = c(0.1, 0.4, 0.43, 0.5, 0.65, 0.7, 0.87)

  #les y doivent etre trié
  coord =   coord = spline(x, y , n = 10*length(x), method = "hyman", xmin = min(x), xmax = max(x),  ties = mean)

  output$vie <- renderText({"Cubic Splines"})});
  output$vie2 = renderPlot({ plot(coord$x, coord$y, type="l")});
})
