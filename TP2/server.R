
library(shiny);

##########################################################
################# TP2 ####################################
##########################################################


#tabtauxInit: liste des vrai taux initale
#E: tableau des vrai taux,nb periode
#S: tableau des taux futurs
rate <-function(b, e, real, fict){
    tmp = 0;
    prod = 1;
    for (i in (b:e)){
        prod = prod * (1 + fict[[i]]);
    }
    tmp = ((1+real[e])**e)/prod -1;
    return (tmp);
}



#La collection des taux futurs la suite(ri^n)
#E: vrai taux initial, tableaux des taux fictifs, nb periode
#S: tableuax des taux futurs
futur <- function(real, b, n){
    fict = list();
    for(i in (1:b)){
        fict = append(fict, 0);
    }
    fict = append(fict, real[b]);
    for (i in (b:n)){
        tmp = rate(1, i, real, fict);
        fict = append(fict, tmp);
    }
    #tab = matrix(unlist(fict));
    return(fict);
}


#Triangle des taux
triangle <- function(real, b, n){
    l = length(real);
    futur = futur(real,1,l);
    for (i in (2:l)){
        futur = rbind(futur, futur(real,i,l))
    }

    #tab = matrix(unlist(liste));
    return(futur);
}



shinyServer(function(input, output) {
  #output$vie <- renderText({
   # anu(input$cap, input$taux1, input$maturite, input$periode);
    #})

  real <- c(0.01, 0.02, 0.03, 0.04, 0.08);
  #real <- c(input$taux, input$taux1, input$taux2, input$taux3);

    #output$vie <- "aaaa";
    output$vie1<- renderTable({
      triangle(real, input$debut, input$periode);
      })
  })
