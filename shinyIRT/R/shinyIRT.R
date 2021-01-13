
shinyIRT<-function(){


  ui <- dashboardPage(skin = "blue",
                      dashboardHeader(title = "ITEM RESPONSE THEORY ANALYSIS",titleWidth = 500),
                      dashboardSidebar(
                        sidebarMenu(id="sidebarmenu",
                                    menuItem("About Package",tabName = "info",icon = icon("info-circle")),
                                    menuItem("Browse",tabName = "upload",icon = icon("upload")),
                                    menuItem("Model Fit",tabName = "modelfit",icon = icon("thumbs-up")),
                                    menuItem("Item Parameter Estimation",tabName = "param",icon = icon("cogs")),

                                    menuItem("Ability Estimation",tabName = "theta",icon = icon("graduation-cap")),
                                    menuItem("IRT Assumptions",textOutput("new"),startExpanded = FALSE,icon = icon("bar-chart"),

                                             menuSubItem("Dimensionality",tabName = "boyut",icon = icon("pie-chart"))) ,
                                    menuItem(tabName="karakteristik","Item Characteristic Curve",startExpanded = FALSE, icon = icon("line-chart"))


                        )),
                      dashboardBody(
                        tabItems(
                          tabItem(tabName = "info",
                                  fluidRow(

                                    box(title = "About Package", solidHeader = TRUE, status = "info",htmlOutput("info"),width = 12)
                                  )),
                          tabItem(tabName = "upload",
                                  fluidRow(
                                    box(title = "Browse Data",status = "warning",solidHeader = TRUE,width = 5,
                                        fileInput("dataset", ".xls or .xlsx. First row as item names", placeholder="File",buttonLabel = "Add",accept = c("xlsx","xls","csv")
                                        )),
                                    box(title="Data Preview",solidHeader = TRUE,dataTableOutput("preview"),status = "info",width = 12)


                                  )),
                          tabItem(
                            tabName = "param",
                            fluidRow(
                              box(title="Item Parameter Estimations",dataTableOutput("param")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5), solidHeader = TRUE,status = "info",width = 10),
                              box(title="Choose IRT Model",solidHeader = TRUE,status = "warning",width = 2,
                                  uiOutput(outputId="modselect"),submitButton("Hesapla"))

                            )),
                          tabItem(
                            tabName = "modelfit",
                            fluidRow(
                              box(title="Fit Indexes",dataTableOutput("modelfit")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5), solidHeader = TRUE,status = "info",width = 10)


                            )),

                          tabItem(
                            tabName = "theta",
                            fluidRow(
                              box(title="Ability Estimations",dataTableOutput("theta")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5), solidHeader = TRUE,status = "info",width = 8),



                            )),

                          tabItem(
                            tabName = "boyut",
                            fluidRow(

                              box(title="Scree Plot",solidHeader = TRUE,status = "info",width = 8,
                                  plotOutput("screeplot")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5)),
                              box(title="Eigen Values",solidHeader = TRUE,status = "info",width = 4,
                                  dataTableOutput("eigens")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5))


                            )
                          ),
                          tabItem(
                            tabName = "yerelb",
                            fluidRow(
                              box(title="Local Dependent Item couples",tableOutput("yerelb")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5),solidHeader = TRUE,status = "info",width = 8),
                              box(title = "Info",status = "success",solidHeader = TRUE,width = 4,"Yen's Q Test")
                            )),



                          tabItem(tabName = "karakteristik",
                                  fluidRow(
                                    box(title="Choose Item",solidHeader = TRUE,status = "warning",width = 12,
                                        uiOutput(outputId="maddeler"),submitButton("view")),
                                    box(title="Item Characteristic Curves",solidHeader = TRUE, plotOutput("karakter")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5),status = "primary",width = 6),
                                    box(title="Item Information Curves",solidHeader = TRUE, plotOutput("bilgi")%>% withSpinner(color="#0dc5c1",type=6,size = 1.5),status = "primary",width = 6)


                                  ))

                        )


                      ))

  server <- function(input, output,session) {

    mydata<-reactive({
      inFile <- input$dataset
      if (is.null(inFile))
        return("Please upload data")
      dataset<- read_xlsx(inFile$datapath, sheet=1)
      data<-as.data.frame(dataset)
      data
    })



    output$maddeler <- renderUI({

      selectInput(inputId="boyutvariable",
                  label="Choose an item",
                  choices=c("All",1:length(mydata())), selected=NULL)
    })

    output$modselect<-renderUI({
      selectInput(inputId ="modselect",selected=NULL,label = NULL,choices = c("Rasch Model","2 PLM","3 PLM","Graded Response Model","Generalized Partial Credit Model"))
    })

    output$thetaselect<-renderUI({
      selectInput(inputId ="thetaselect",label = NULL,choices = c("Rasch Model","2 PLM","3 PLM","Graded Response Model","Generalized Partial Credit Model"))
    })

    output$param<-renderDataTable({
      inFile <- input$dataset
      if (is.null(inFile))
        return(NULL)
      data<- read_xlsx(inFile$datapath, sheet=1)

      if(is.null(input$modselect))
        return(NULL)

      if(input$modselect=="Rasch Model"){


        results.gpcm <- mirt(data, 1, itemtype="Rasch", SE=TRUE, verbose=FALSE)
        coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, printSE=TRUE)
        SE<-c()
        bpar<-c()
        for(i in 1:ncol(data)){
          SE[i]<- coef.gpcm[[i]][2,2]
          bpar[i]<- coef.gpcm[[i]][1,2]
        }
        item<-1:ncol(data)
        table<- cbind(item,bpar,SE)
        tablo<-as.data.frame(table)

        colnames(tablo)<-c("item","b ","SE")
      }


      if(input$modselect=="2 PLM"){
        if(all(data<2)==FALSE)
        {return("Data must be dichotomous in this model.")}

        results.gpcm <- mirt(data, 1, itemtype="2PL", SE=TRUE, verbose=FALSE)
        coef.gpcm <- coef(results.gpcm, IRTpars=TRUE,printSE=TRUE)
        aSE<-c()
        bSE<-c()
        apar<-c()
        bpar<-c()
        for(i in 1:ncol(data)){
          apar[i]<- coef.gpcm[[i]][1,1]
          bpar[i]<- coef.gpcm[[i]][1,2]
          aSE[i]<- coef.gpcm[[i]][2,1]
          bSE[i]<- coef.gpcm[[i]][2,2]
        }
        item<-1:ncol(data)
        tablo<-cbind(item,apar,aSE,bpar,bSE)
        tablo<-as.data.frame(tablo)
        colnames(tablo)<-c("item","a ","SE","b","SE")
      }
      if(input$modselect=="3 PLM"){
        if(all(data<2)==FALSE)
        {return("Data must be dichotomous in this model.")}

        results.gpcm <- mirt(data, 1, itemtype="3PL", SE=TRUE, verbose=FALSE)
        coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, printSE=TRUE)
        aSE<-c()
        bSE<-c()
        cSE<-c()
        apar<-c()
        bpar<-c()
        cpar<-c()
        for(i in 1:ncol(data)){
          apar[i]<- coef.gpcm[[i]][1,1]
          bpar[i]<- coef.gpcm[[i]][1,2]
          cpar[i]<- coef.gpcm[[i]][1,3]
          aSE[i]<- coef.gpcm[[i]][2,1]
          bSE[i]<- coef.gpcm[[i]][2,2]
          cSE[i]<- coef.gpcm[[i]][2,3]
        }

        item<-1:ncol(data)
        tablo<-as.data.frame(cbind(item,apar,aSE,bpar,bSE,cpar,cSE))
        colnames(tablo)<-c("item","a","SE","b ","SE","c ","SE")
      }

      if(input$modselect=="Graded Response Model"){


        results.gpcm <- mirt(data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)
        coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE,SE=TRUE)
        tablo<- coef.gpcm$item

      }
      if(input$modselect=="Generalized Partial Credit Model"){


        results.gpcm <- mirt(data, 1, itemtype="gpcm", SE=TRUE, verbose=FALSE)
        coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE,SE=TRUE)
        tablo<- coef.gpcm$item

      }


      tablo<-as.data.frame(round(tablo,3))
    })

    output$modelfit<-renderDataTable({
      inFile <- input$dataset
      if (is.null(inFile))
        return(NULL)
      data<- read_xlsx(inFile$datapath, sheet=1)


      if(all(data<2)==TRUE){


        results.gpcm <- mirt(data, 1, itemtype="Rasch", SE=TRUE, verbose=FALSE)

        AICrasch<-results.gpcm@Fit$AIC
        BICrasch<-results.gpcm@Fit$BIC
        loglikelihoodrasch<-results.gpcm@Fit$logLik

        raschfit<-c(AICrasch,BICrasch,loglikelihoodrasch)



        results.gpcm <- mirt(data, 1, itemtype="2PL", SE=TRUE, verbose=FALSE)

        AIC2pl<-results.gpcm@Fit$AIC
        BIC2pl<-results.gpcm@Fit$BIC
        loglikelihood2pl<-results.gpcm@Fit$logLik

        ikiplfit<-c(AIC2pl,BIC2pl,loglikelihood2pl)

        results.gpcm <- mirt(data, 1, itemtype="3PL", SE=TRUE, verbose=FALSE)

        AIC3pl<-results.gpcm@Fit$AIC
        BIC3pl<-results.gpcm@Fit$BIC
        loglikelihood3pl<-results.gpcm@Fit$logLik

        ucplfit<-c(AIC3pl,BIC3pl,loglikelihood3pl)


        index<-as.data.frame(rbind(raschfit,ikiplfit,ucplfit))

        name<-c("Rasch","2PLM","3PLM")
        index<-as.data.frame(cbind(name,index))
        colnames(index)<-c("Model","AIC","BIC","loglikelihood")
        index
      }

      if(all(data<2)==FALSE){


        results.gpcm <- mirt(data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)

        AICgraded<-results.gpcm@Fit$AIC
        BICgraded<-results.gpcm@Fit$BIC
        loglikelihoodgraded<-results.gpcm@Fit$logLik

        gradedfit<-c(AICgraded,BICgraded,loglikelihoodgraded)



        results.gpcm <- mirt(data, 1, itemtype="gpcm", SE=TRUE, verbose=FALSE)

        AICgpcm<-results.gpcm@Fit$AIC
        BICgpcm<-results.gpcm@Fit$BIC
        loglikelihoodgpcm<-results.gpcm@Fit$logLik

        gpcmfit<-c(AICgpcm,BICgpcm,loglikelihoodgpcm)




        index<-as.data.frame(rbind(gradedfit,gpcmfit))

        name<-c("Graded Response Model","GPCM")
        index<-as.data.frame(cbind(name,index))
        colnames(index)<-c("Model","AIC","BIC","loglikelihood")
        index
      }

      index

    })



    output$theta<-renderDataTable({
      inFile <- input$dataset
      if (is.null(inFile))
        return(NULL)
      data<- read_xlsx(inFile$datapath, sheet=1)
      if(input$modselect=="Rasch Model"){

        results.gpcm <- mirt(data, 1, itemtype="Rasch", SE=TRUE, verbose=FALSE)
        abilities<- round(fscores(results.gpcm,full.scores.SE = TRUE),4)
        personid<-1:nrow(data)
        abilities<-cbind(personid,abilities)
        colnames(abilities)<-c("Person id","Theta","SE")
      }


      if(input$modselect=="2 PLM"){



        results.gpcm <- mirt(data, 1, itemtype="2PL", SE=TRUE, verbose=FALSE)
        abilities<- round(fscores(results.gpcm,full.scores.SE = TRUE),4)

        personid<-1:nrow(data)
        abilities<-cbind(personid,abilities)
        colnames(abilities)<-c("Person id","Theta","SE")
      }
      if(input$modselect=="3 PLM"){



        results.gpcm <- mirt(data, 1, itemtype="3PL", SE=TRUE, verbose=FALSE)
        abilities<- round(fscores(results.gpcm,full.scores.SE = TRUE),4)
        personid<-1:nrow(data)
        abilities<-cbind(personid,abilities)
        colnames(abilities)<-c("Person id","Theta","SE")
      }

      if(input$modselect=="Graded Response Model"){


        results.gpcm <- mirt(data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)
        abilities<- round(fscores(results.gpcm,full.scores.SE = TRUE),4)
        personid<-1:nrow(data)
        abilities<-cbind(personid,abilities)
        colnames(abilities)<-c("Person id","Theta","SE")
      }
      if(input$modselect=="Generalized Partial Credit Model"){


        results.gpcm <- mirt(data, 1, itemtype="gpcm", SE=TRUE, verbose=FALSE)
        abilities<- round(fscores(results.gpcm,full.scores.SE = TRUE),4)
        personid<-1:nrow(data)
        abilities<-cbind(personid,abilities)
        colnames(abilities)<-c("Person id","Theta","SE")

      }

      abilities

    })

    output$yerelb<-renderTable({
      inFile <- input$dataset
      if (is.null(inFile))
        return(NULL)
      Response<- read_xlsx(inFile$datapath, sheet=1)
      if(all(Response<2)==FALSE)


        a<-est(Response, model = "2PL", engine = "ltm")
      disc<-as.vector(a[[1]][,1])
      bpar<-as.vector(a[[1]][,2])
      Response<-as.matrix(Response)
      yetenek<-ability(resp= Response, ip= a , method = "WLE", mu = 0, sigma = 1, n = 5)
      yetenek<-as.vector(yetenek[,1])

      logis<- function(yetenek, bpar, disc) {

        prob<-1/(1+exp(-(1.7)*disc*(yetenek-bpar)))
        return(prob)

      }
      olasiliklar<-matrix(NA,ncol = ncol(Response), nrow = nrow(Response))

      for (i in 1:ncol(Response)){
        for (j in 1:nrow(Response)){
          olasiliklar[j,i]<-logis(yetenek[j],bpar[i],disc[i])
        }
      }

      residuals<-Response-olasiliklar

      cormatrix<-matrix(NA,ncol = ncol(Response),nrow = nrow(Response))
      cormatrix<-stats::cor(residuals,use="pairwise.complete.obs")
      madde<-ncol(Response)
      itempairs<-t(utils::combn(madde,2))
      nares<-1- is.na(residuals)
      NIP<-crossprod(nares)
      pairlist<-matrix(NA,nrow = (madde*(madde-1)/2),ncol=4)
      pairlist[,1]<-colnames(cormatrix)[itempairs[,1]]
      pairlist[,2]<-colnames(cormatrix)[itempairs[,2]]
      pairlist[,3]<-round(cormatrix[itempairs],3)
      Yorum<-c()

      for(i in 1:(madde*(madde-1)/2)){

        if(abs(cormatrix[itempairs][i])<0.20  ){
          Yorum[i]<-"Local Independent"
        }
        if(abs(cormatrix[itempairs][i])>0.20){
          Yorum[i]<-"Local Dependency Detected"
        }
      }
      pairlist[,4]<-NIP[itempairs]


      pairlist<-as.data.frame(pairlist)
      pairlist[,3]<-as.numeric(paste(pairlist[,3]))

      colnames(pairlist)<-c("1. item", "2. item" , "cor","N")
      pairlist<-cbind(pairlist,Yorum)

      aaa<- pairlist[pairlist[,5] == "Local Dependency Detected",]

      aaa<-aaa[,-4]
      aaa
    })

    output$screeplot<-renderPlot({
      inFile <- input$dataset
      if (is.null(inFile))
        return(NULL)
      Response<- read_xlsx(inFile$datapath, sheet=1)
      fa.parallel(Response, fm = 'minres', fa = 'fa')

    })

    output$eigens<-renderDataTable({
      inFile <- input$dataset


      inFile <- input$dataset
      if (is.null(inFile))
        return(NULL)
      Response<- read_xlsx(inFile$datapath, sheet=1)
      a<-fa.parallel(Response, fm = 'minres', fa = 'fa')

      sayi<-1:ncol(Response)
      sonuc<-as.data.frame(cbind(sayi,round(a[[1]],4)))
      colnames(sonuc)<-c("Factor No","Eigen Value")
      sonuc

    })

    output$bilgi<-renderPlot({
      inFile <- input$dataset
      if (is.null(inFile))
        return(NULL)
      data<- read_xlsx(inFile$datapath, sheet=1)
      if(is.null(input$boyutvariable))
        return(NULL)


      if(input$modselect=="Rasch Model"){

        results.gpcm <- mirt(data, 1, itemtype="Rasch", SE=TRUE, verbose=FALSE)

        if(input$boyutvariable=="All"){
          plot<- plot(results.gpcm, type = "info", which.items = 1:ncol(data), theta_lim=c(-3,3))
        }

        else{ plot<- itemplot(results.gpcm,item = as.numeric(input$boyutvariable),type = "info",theta_lim = c(-4,4))}
      }


      if(input$modselect=="2 PLM"){



        results.gpcm <- mirt(data, 1, itemtype="2PL", SE=TRUE, verbose=FALSE)
        if(input$boyutvariable=="All"){
          plot<- plot(results.gpcm, type = "info", which.items = 1:ncol(data), theta_lim=c(-3,3))
        }

        else{ plot<- itemplot(results.gpcm,item = as.numeric(input$boyutvariable),type = "info",theta_lim = c(-4,4))}
      }
      if(input$modselect=="3 PLM"){



        results.gpcm <- mirt(data, 1, itemtype="3PL", SE=TRUE, verbose=FALSE)
        if(input$boyutvariable=="All"){
          plot<- plot(results.gpcm, type = "info", which.items = 1:ncol(data), theta_lim=c(-3,3))
        }

        else{ plot<- itemplot(results.gpcm,item = as.numeric(input$boyutvariable),type = "info",theta_lim = c(-4,4))}
      }

      if(input$modselect=="Graded Response Model"){


        results.gpcm <- mirt(data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)
        if(input$boyutvariable=="All"){
          plot<- plot(results.gpcm, type = "info", theta_lim=c(-3,3))
        }

        else{ plot<- itemplot(results.gpcm,item = as.numeric(input$boyutvariable),type = "info",theta_lim = c(-4,4))}
      }
      if(input$modselect=="Generalized Partial Credit Model"){


        results.gpcm <- mirt(data, 1, itemtype="gpcm", SE=TRUE, verbose=FALSE)

        if(input$boyutvariable=="All"){
          plot<- plot(results.gpcm, type = "info", theta_lim=c(-3,3))
        }

        else{ plot<- itemplot(results.gpcm,item = as.numeric(input$boyutvariable),type = "info",theta_lim = c(-4,4))}

      }
      plot

    })

    output$karakter<-renderPlot({
      inFile <- input$dataset
      if (is.null(inFile))
        return(NULL)
      data<- read_xlsx(inFile$datapath, sheet=1)
      if(is.null(input$boyutvariable))
        return(NULL)


      if(input$modselect=="Rasch Model"){

        results.gpcm <- mirt(data, 1, itemtype="Rasch", SE=TRUE, verbose=FALSE)

        if(input$boyutvariable=="All"){
          plot<- plot(results.gpcm, type = "trace", which.items = 1:ncol(data), theta_lim=c(-3,3))
        }

        else{ plot<- itemplot(results.gpcm,item = as.numeric(input$boyutvariable),type = "trace", theta_lim=c(-3,3))}
      }


      if(input$modselect=="2 PLM"){



        results.gpcm <- mirt(data, 1, itemtype="2PL", SE=TRUE, verbose=FALSE)
        if(input$boyutvariable=="All"){
          plot<- plot(results.gpcm, type = "trace", which.items = 1:ncol(data), theta_lim=c(-3,3))
        }
        else {plot<- itemplot(results.gpcm,item = as.numeric(input$boyutvariable),type = "trace", theta_lim=c(-3,3))}
      }
      if(input$modselect=="3 PLM"){



        results.gpcm <- mirt(data, 1, itemtype="3PL", SE=TRUE, verbose=FALSE)
        if(input$boyutvariable=="All"){
          plot<- plot(results.gpcm, type = "trace", which.items = 1:ncol(data), theta_lim=c(-3,3))
        }
        else { plot<- itemplot(results.gpcm,item = as.numeric(input$boyutvariable),type = "trace", theta_lim=c(-3,3))}
      }

      if(input$modselect=="Graded Response Model"){


        results.gpcm <- mirt(data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)
        if(input$boyutvariable=="All"){
          plot<- plot(results.gpcm, type = "trace", which.items = 1:ncol(data), theta_lim=c(-3,3))
        }
        else{ plot<- itemplot(results.gpcm,item = as.numeric(input$boyutvariable),type = "trace", theta_lim=c(-3,3))}
      }
      if(input$modselect=="Generalized Partial Credit Model"){


        results.gpcm <- mirt(data, 1, itemtype="gpcm", SE=TRUE, verbose=FALSE)
        if(input$boyutvariable=="All"){
          plot<- plot(results.gpcm, type = "trace", which.items = 1:ncol(data), theta_lim=c(-3,3))
        }

        else{plot<- itemplot(results.gpcm,item = as.numeric(input$boyutvariable),type = "trace", theta_lim=c(-3,3))}

      }
      plot

    })

    output$info<-renderText({
      paste(p(strong('Package:'), "IRTshiny"),p(strong('Background Packages:'), "mirt,","irtoys,","psych"),
            p(strong('Package Description:'), "Performing IRT analysis such as paramater estimation, ability estimation, item and model fit analyse, local independence assumption, dimensionality assumption, charachteristic and information curves under various models with a user friendly shiny interface."),

            p(strong('Package Author:'), "Huseyin Yildiz"),
            p(strong('e-mail:'), tags$a(href="mailto:huseyinyildiz35@gmail.com", "huseyinyildiz35@gmail.com"))

      )
    })

    output$preview<-renderDataTable({

      mydata()
    })


  }

  shinyApp(ui, server)


}

