

library(shiny)
library(deSolve)
library(ggplot2)
library(patchwork)
library(reshape2)
library(viridis)
library(knitr)

source("SIRCODE.R",local=TRUE)
source("OutputCalculator.R",local=TRUE)
source("cm.R",local=TRUE)

ui <- fluidPage(
    titlePanel("What to do in a pandemic?"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("CM",
                     "Contact Matrix",
                     c("Generic" = "Contact1",
                       "Young/Middle interact more" = "Contact2",
                       "Young/Old interact more" = "Contact3"
                     )),
        
        radioButtons("vacrate", 
                     "Annual Vaccine Allocation:",
                     c("25%" = 0.287,
                       "50%" = 0.69,
                       "75%" = 1.38)),
        
        radioButtons("R0", "R0:",
                     c("1.15" = 1.15,
                       "2.50" = 2.50,
                       "7.00" = 7.00)),
        
        radioButtons("tran", "Reduces transmissibility:",
                     c("Effectively" = 0.05,
                       "Ineffectively" = 0.8)),
        radioButtons("sus", "Reduces susceptibility:",
                     c("Effectively" = 0.05,
                       "Ineffectively" = 0.8))),
       mainPanel(
        # Show a plot of the generated distribution
          tabsetPanel(type = "tabs",
           tabPanel("Dynamics", fluid=TRUE,
                
                               h3("The Dynamics (Susceptible, Infected, and Recovered)"),
                               p(style = "text-align: justify; font-size = 12px",
                                 "This is a simple SIR model (you can check the Equation 
                                 Tab for more information). We have the dynamics of
                                 the susceptible, infected, and recovered for both vaccinated
                                 and unvaccinated individuals. The blue represents
                                 vaccinated individuals while the red represents unvaccinated individuals."),
                               
                               plotOutput("distPlot"),
                               p(style = "text-align: justify; font-size = 12px",
                                 "Using the Expected Value of Perfect Information (EVPI),
                                 we explore how uncertainty can influence the decision making progress
                                 of prioritizing vaccines. 
                                 
                                 The uncertainties in our model are how well the vaccine
                                 reduces one's susceptibility
                                 and/or transmissibility of the novel disease. With these 
                                 uncertainties, what is the best strategy:
                                 vaccinating the youngest, middle, or oldest age 
                                 first?")),
                     
                       tabPanel("Outputs", 
                                
                                plotOutput("tileplot"),
                               p(style = "text-align: justify; font-size = 16px",
                                 "These are the total caseload and mortality a year later from our model
                                 simulation. In the model, 
                                 we assume elderly indvidiuals suffer the highest mortality.
                                 On the y-axis, we have the different nature of the vaccines.
                                 and on the x-axis, we have the three prioritization: young, middle, and old. The
                                 red boxes represent the minimum cases for each vaccine scenario. ")),
                      
                    tabPanel("Contact Matrix",
                             h3("Contact Matrix"),
                             p(style = "text-align: justify; font-size = 16px",
                               "COVID-19 relies on close contact to be transmitted- the contact
                               matrices can drive the dynamics of the disease especially 
                               within different age groups. Understanding the contact
                               matrices between different age classes can provide insight into
                               how prioritizing certain age groups may change the outcome
                               of the disease"),
                             plotOutput("contactmatrix")),
           tabPanel("Equations",uiOutput('markdown'))))
             
           
           ))




server <- function(input, output) {
  
  
  ##########################################
  ##########################################
  ##########################################

  output$distPlot <- renderPlot({

      parms1 <-
        list(a =  rep(1/26,3),#ageing in and out 
        mu = 1/80, #mortality
        gamma = 1/7, #recovery time
        R0 =as.numeric(input$R0),#as.numeric(input$R0), #R
        sus = as.numeric(input$sus), #remove
        tran = as.numeric(input$tran),
        r = as.numeric(input$vacrate),
        W = get(input$CM),
        inc = 1)
      
      parms2  <- list(
        a =  rep(1/26,3),#ageing in and out 
        mu = 1/80, #mortality
        gamma = 1/7, #recovery time
        R0 = as.numeric(input$R0), #R
        sus = as.numeric(input$sus), #remove
        tran = as.numeric(input$tran),
        r = as.numeric(input$vacrate),
        W =get(input$CM),
        inc = 2)
    
      parms3  <-list(
        a =  rep(1/26,3),#ageing in and out 
        mu = 1/80, #mortality
        gamma = 1/7, #recovery time
        R0 = as.numeric(input$R0), #R
        sus = as.numeric(input$sus), #remove
        tran = as.numeric(input$tran),
        r = as.numeric(input$vacrate),
        W =get(input$CM),
        inc = 3)
    
  
        dat1= as.data.frame(lsoda(y_initial, times=times,
                             func =SIR_evpi  ,
                             parms = parms1,atol= 1e-6, rtol=1e-7))
    
        dat2= as.data.frame(lsoda(y_initial, times=times,
                              func =SIR_evpi  ,
                              parms = parms2,atol= 1e-6, rtol=1e-7))
    
        dat3= as.data.frame(lsoda(y_initial, times=times,
                              func =SIR_evpi  ,
                              parms = parms3,atol= 1e-6, rtol=1e-7))

###Susceptible Graphs
Sdat1 <- ggplot(dat1, aes(x = time, y=rowSums(dat1[,2:4])))+
  geom_line(color='#e81817',size=0.8)+
  geom_line(data = dat1, aes(x = time, y= rowSums(dat1[,5:7])),
            color='#17e7e8', size =0.8)+theme_classic()+
  xlab("Time (Year)")+
  ylab("Susceptibles \n \n Proportion")+
  ggtitle("Young")+theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))

Sdat2 <- ggplot(dat2, aes(x = time, y=rowSums(dat2[,2:4])))+
  geom_line(color='#e81817',size=0.8)+
  geom_line(data = dat2, aes(x = time, y= rowSums(dat2[,5:7])),
            color='#17e7e8', size =0.8)+theme_classic()+
  xlab("Time (Year)")+
  ylab("Proportion")+
  ggtitle("Middle Age")+theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))


Sdat3 <- ggplot(dat3, aes(x = time, y=rowSums(dat3[,2:4])))+
  geom_line(color='#e81817',size=0.8)+
  geom_line(data = dat3, aes(x = time, y= rowSums(dat3[,5:7])),
            color='#17e7e8', size =0.8)+theme_classic()+
  xlab("Time (Year)")+
  ylab("Proportion")+
  ggtitle("Old")+ theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))


###Infeced Graphs
Idat1 <- ggplot(dat1, aes(x = time, y=rowSums(dat1[,8:10])))+
  geom_line(color='#e81817',size=0.8)+
  geom_line(data = dat1, aes(x = time, y= rowSums(dat1[,11:13])),
            color='#17e7e8',size=0.8)+theme_classic() + xlab("Time (Years)")+
  ylab("Infected \n \n Proportion")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))

Idat2 <- ggplot(dat2, aes(x = time, y=rowSums(dat2[,8:10])))+
  geom_line(color='#e81817',size=0.8)+
  geom_line(data = dat2, aes(x = time, y= rowSums(dat2[,11:13])),
            color='#17e7e8',size=0.8)+theme_classic() + xlab("Time (Years)")+
  ylab("Proportion")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))

Idat3 <- ggplot(dat3, aes(x = time, y=rowSums(dat3[,8:10])))+
  geom_line(color='#e81817',size=0.8)+
  geom_line(data = dat3, aes(x = time, y= rowSums(dat3[,11:13])),
            color='#17e7e8',size=0.8)+theme_classic()+
  xlab("Time (Years)")+
  ylab("Proportion")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))

###Recovered


Rdat1 <- ggplot(dat1, aes(x = time, y=rowSums(dat1[,14:16])))+
  geom_line(color='#5ca388',size = 0.8)+
  xlab("Time (Months)")+
  ylab("Recovered \n \n Proportion")+theme_classic()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))

Rdat2 <- ggplot(dat2, aes(x = time, y=rowSums(dat2[,14:16])))+
  geom_line(color='#5ca388',size=0.8)+
  xlab("Time (Months)")+
  ylab("Proportion")+theme_classic()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))


Rdat3 <- ggplot(dat3, aes(x = time, y=rowSums(dat3[,14:16])))+
  geom_line(color='#5ca388',size=0.8)+
  xlab("Time (Months)")+
  ylab("Proportion")+theme_classic()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))

    (Sdat1+ Sdat2 + Sdat3)/
    (Idat1 + Idat2 + Idat3)/
    (Rdat1 + Rdat2 + Rdat3)
  

    })
    
    
    output$tileplot <- renderPlot({
      
      sus_trans <- expand.grid(sus = c(0.05,1),
                               tran = c(0.05,1),
                               inc = seq(1,3))
      
      labeler <- rep(c("S:Effective-T:Effective",
                       "S:Ineffective-T:Effective",
                       "S:Effective-T:Ineffective",
                       "S:Ineffective-T:Ineffective"),3)
      
      
      parameters<- NULL
      for (k in seq(1, nrow(sus_trans))){
      parameters[[k]] <- 
        
       list(
          a =  rep(1/26,3),#ageing in and out 
          mu = 1/80, #mortality
          gamma = 1/7, #recovery time
          R0 = as.numeric(input$R0), #R
          sus =     sus_trans[k,1] , #remove
          tran =  sus_trans[k,2],
          r = as.numeric(input$vacrate),
          W =get(input$CM),
          inc =  sus_trans[k,3])
      
      }
      
      dat2 <- NULL
      for (k in seq(1, nrow(sus_trans))){
      
        tmp= as.data.frame(lsoda(y_initial, times=times,
                                func =SIR_evpi  ,
                parms =    parameters[[k]],atol= 1e-6, rtol=1e-7))
      
      dat2[[k]] = data.frame(t(output_calulator(tmp, 13)),sus_trans[k,],labeler[k])
      
      }
      
      dat3 <- do.call(rbind, dat2)
      
     colnames(dat3) = c("cases",'mort','sus','tran',
                         'inc','labeler')
      
     splitted_dat <- split(dat3, list( dat3$labeler))
     
     Min_NULL <- NULL
     for(k in seq(1,length(splitted_dat))){
       tmp <- splitted_dat[[k]]
       Min_Cases<- tmp[which.min(tmp$cases),]
       Min_Mort<- tmp[which.min(tmp$mort),]
       
       tmp$cases_id<- ifelse(tmp$cases == Min_Cases$cases,"Min","Not")
       tmp$mort_id<- ifelse(tmp$cases == Min_Mort$cases,"Min","Not")
       
         Min_NULL[[k]] <- tmp
       }
     dat4 <- do.call(rbind,Min_NULL)
     
     
     
      
      graph1 <- ggplot(dat4, aes(x = inc, y= labeler,
                                 fill = cases)
                               )+
        geom_tile(aes(color=cases_id,   size =cases_id), width = 0.95, height = 0.92)+
        scale_color_manual(values=c("red","black"),guide="none")+
        scale_size_manual(values=c(1.2,0),guide="none")+
        geom_label(fill='white', aes(label = round(cases,3)))+

        scale_fill_viridis(name='Cases',option='viridis')+
        scale_x_discrete(expand=c(0,0),
                         labels = c("Young","Middle","Old"))+
        scale_y_discrete(expand=c(0,0))+ theme_classic()+
        xlab("Actions")+
        ylab("Nature of the vaccine")+
        guides(colors = FALSE)
        

      graph2 <- 
        ggplot(dat4, aes(x = inc, y= labeler,
                         fill = mort)
        )+
        geom_tile(aes(color=mort_id,   size =mort_id), width = 0.95, height = 0.92)+
        scale_color_manual(values=c("red","black"),guide="none")+
        scale_size_manual(values=c(1.2,0),guide="none")+
        geom_label(fill='white', aes(label = round(mort,3)))+
        
        scale_fill_viridis(name='Deaths',option='inferno')+
        scale_x_discrete(expand=c(0,0),
                         labels = c("Young","Middle","Old"))+
        scale_y_discrete(expand=c(0,0))+ theme_classic()+
        xlab("Actions")+
        ylab("Nature of the vaccine")+
        guides(colors = FALSE)
        
      
      graph1 / graph2
      
      
      
    })    
    
    
    
    
    output$contactmatrix <- renderPlot({
      
      melted_contact1 <- melt(Contact1)
      melted_contact2 <- melt(Contact2)
      melted_contact3 <- melt(Contact3)
      
     cm1<-  ggplot( melted_contact1, aes(x=as.factor(Var1),y=as.factor(Var2),
                                   fill=value))+geom_tile()+
       scale_fill_viridis(option='inferno')+
       scale_x_discrete(expand=c(0,0),labels = c("Y","M","O"))+
       scale_y_discrete(expand=c(0,0),labels = c("Y","M","O"))+coord_equal()+
       ggtitle("Generic")+xlab("Age group")+ylab("Age group")
     
     cm2<-  ggplot( melted_contact2, aes(x=as.factor(Var1),y=as.factor(Var2),
                                         fill=value))+geom_tile()+
       scale_fill_viridis(option='inferno')+
       scale_x_discrete(expand=c(0,0),labels = c("Y","M","O"))+
       scale_y_discrete(expand=c(0,0),labels = c("Y","M","O"))+coord_equal()+
         ggtitle("The Young/Middle Aged")+xlab("Age group")+ylab("Age group")
     
     cm3<-  ggplot( melted_contact3, aes(x=as.factor(Var1),y=as.factor(Var2),
                                         fill=value))+geom_tile()+
       scale_fill_viridis(option='inferno')+
       scale_x_discrete(expand=c(0,0),labels = c("Y","M","O"))+
       scale_y_discrete(expand=c(0,0),labels = c("Y","M","O"))+coord_equal()+
       ggtitle("The Young/Elderly ")+xlab("Age group")+ylab("Age group")
     
     cm1 + cm2 + cm3 + plot_layout(guides = "collect")
     
    },height = 400, width =600)   
    
    
    
    
    
    
    
    
    
    
    
    
      
      output$markdown <- renderUI({
        HTML(markdown::markdownToHTML(knit('equations.rmd', quiet = TRUE)))
      })
      
     
}

# Run the application 
shinyApp(ui = ui, server = server)
