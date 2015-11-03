# server.R
#library(lattice)
#library(rgl)

shinyServer(function(input, output) {
  
  myOutput <- reactive({
     
    #Ena=50; Ek=-77; El=-54.4; gna=1.20; gk=0.36; gl=0.003; C=0.01 ; 
    #Tstart=0;Tstop=55; dt=0.025
    if(input$whatClamp=="current"){
      
      TBase=input$BaselineDuration; TClamp=input$ClampDuration; TEnd=input$EndDuration; dt=input$dt;
      
      
      times = seq(from=0, to = TClamp+TEnd+TBase, by = dt)
      realTime = vector(mode="integer",length=length(times))
      Vnet=vector(mode="integer",length=length(times))
      m=vector(mode="integer",length=length(times))
      h=vector(mode="integer",length=length(times))
      n=vector(mode="integer",length=length(times))
      mkca=vector(mode="integer",length=length(times))
      mAHP=vector(mode="integer",length=length(times))
      mM=vector(mode="integer",length=length(times))
      m1A= vector(mode="integer",length=length(times))
      h1A= vector(mode="integer",length=length(times))
      m2A= vector(mode="integer",length=length(times))
      h2A= vector(mode="integer",length=length(times))
      mT= vector(mode="integer",length=length(times))
      hT= vector(mode="integer",length=length(times))
      mL= vector(mode="integer",length=length(times))
      
      ### change here     
      iKLeak=vector(mode="integer",length=length(times))   
      iNaLeak=vector(mode="integer",length=length(times))    
      iNa=vector(mode="integer",length=length(times))
      iK=vector(mode="integer",length=length(times))
      iCl=vector(mode="integer",length=length(times))
      iCal=vector(mode="integer",length=length(times))
      iKca=vector(mode="integer",length=length(times))
      iA=vector(mode="integer",length=length(times))
      iT=vector(mode="integer",length=length(times))
      iAHP=vector(mode="integer",length=length(times))
      iM=vector(mode="integer",length=length(times))
      Inet=vector(mode="integer",length=length(times))
      
      gKleak=vector(mode="integer",length=length(times))      
      gNaleak=vector(mode="integer",length=length(times))            
      gnareal=vector(mode="integer",length=length(times))
      gkreal=vector(mode="integer",length=length(times))
      gclreal=vector(mode="integer",length=length(times))
      gahpreal=vector(mode="integer",length=length(times))
      gkcareal=vector(mode="integer",length=length(times))
      gareal=vector(mode="integer",length=length(times))
      gtreal=vector(mode="integer",length=length(times))
      gmreal=vector(mode="integer",length=length(times))
      gcalreal=vector(mode="integer",length=length(times))
      
      
      #Ena=input$Ena; Ek=input$Ek; Ec=input$Ec;
      OutNa=input$OutNa
      OutK=input$OutK
      OutCl=input$OutCl
      OutCa=input$OutCa
      InNa=input$InNa
      InK=input$InK
      InCl=input$InCl
      InCa=input$InCa
      
      R=8314
      Temp=273.16+25
      Faraday=9.648*10^4
      
      Ena=((8314*(273.16+25))/(1*9.648*10^4))*log(OutNa/InNa) 
      Ek=((8314*(273.16+25))/(1*9.648*10^4))*log(OutK/InK) 
      Ecl=((8314*(273.16+25))/(-1*9.648*10^4))*log(OutCl/InCl) 
      Eca=((8314*(273.16+25))/(-1*9.648*10^4))*log(OutCa/InCa) 
      ENaleak= 45 
      EKleak= -105
        
      gNaleak=input$GNaleak;gKleak=input$GKleak; gna=input$Gna; gk=input$Gk; gCl=input$Gcl;gCal=input$Gcal;gKca=input$Gkca;gA=input$Ga;gAHP=input$Gahp;gT=input$Gt;gM=input$Gm;
      C=input$C ; 
      
      
      Vnet[1]=(gk*Ek+gna*Ena+gCl*Ecl+gCal*Eca+gKca*Ek+gT*Eca+gA*Ek+gAHP*Ek+gM*Ek+gKleak*EKleak+gNaleak*ENaleak)/(gna+gk+gCl+gCal+gKca+gA+gAHP+gT+gM+gKleak+gNaleak);
      realTime[1]= 0 
      
      m[1]=0.052 ;h[1]=0.596; n[1]=0.317;mAHP[1]=0.1;mkca[1]=0.1;mM[1]=0.1; m1A[1]=0.1;h1A[1]=0.1;m2A[1]=0.1;h2A[1]=0.1;mT[1]=0.01;hT[1]=0.01;mL[1]=0.01; Inet[1]=0;
      ## change here
      
      am <- function(v) 0.1*(v+40)/(1-exp(-(v+40)/10))
      bm <- function(v) 4*exp(-(v+65)/18)
      ah <- function(v) 0.07*exp(-(v+65)/20)
      bh <- function(v) 1/(1+exp(-(v+35)/10))
      an <- function(v) 0.01*(v+55)/(1-exp(-(v+55)/10))
      bn <- function(v) 0.125*exp(-(v+65)/80)
      amAHP = (1.2*10^9)*(InCa)^2      
      bmAHP = 0.01 
      amkca <- function(v) (2.5*10^5)*(InCa)*exp(v/24)
      bmkca <- function(v) 0.1*exp(-v/24)
      minfM <- function(v) 1/(1+exp((-v+35)/10))
      mtauM <- function(v) 1000 / (3.3*exp((v+35)/20)+exp((-v+35)/20))
    
      m1infA <- function(v) 1/(1+exp((v+60)/-8.5))
      m1tauA <- function(v) 1/ (exp((v+35.82)/19.69)+exp((v+79.69)/-12.7)+0.37)
      h1infA <- function(v) 1/(1+exp((v+78)/6))
      h1tauA <- function(v) { if (v< -63) { return ( 1/(exp((v+46.05)/5)+exp((v+238.4)/-37.45)))  } else { return(19) }}
      
      m2infA <- function(v) 1/(1+exp((v+36)/-20))
      m2tauA <- function(v) 1/ (exp((v+35.82)/19.69)+exp((v+79.69)/-127)+0.37)
      h2infA <- function(v) 1/(1+exp((v+78)/6))
      h2tauA <- function(v) { if (v< -73) { return ( 1/(exp((v+46.05)/5)+exp((v+238.4)/-37.45)))  } else { return(60) }} 
      
      minfT <- function(v) 1/(1+exp((v+60.5)/-6.2))
      mtauT <- function(v) 1/ (exp((v+131.6)/-16.7)+exp((v+16.8)/18.2)+0.612)
      hinfT <- function(v) 1/(1+exp((v+84.5)/4.03))
      htauT <- function(v) { if (v< -80) { return ( exp((v+467)/66.6))  } else { return(  exp((v+21.88)/-10.52)+28 ) }}
      
      amCal <- function(v) 1.6/(1+exp(-0.072*(v+5)))
      bmCal <- function(v) 0.02*(v-1.3)/(exp((v-1.31)/5.36)-1)
      
      # change here
      
      
      for(i in seq(2,length(times))){            
        if(times[i]>TBase & times[i]<(TBase+TClamp)){
          Inet[i] <-  input$Iapplied
        }
        
        realTime[i] <-  dt * i 
        gnareal[i]<-gna*h[i-1]*m[i-1]^3
        gkreal[i]<-gk*n[i-1]^4
        gclreal[i]<-gCl*m[i-1]
        gahpreal[i]<-gAHP*mAHP[i-1]^2
        gareal[i]<-gA*(0.6*m1A[i-1]^4*h1A[i-1]+0.4*m2A[i-1]^4*h2A[i-1])
        gtreal[i]<-gT*mT[i-1]
        gmreal[i]<-gM*mM[i-1]
        gcalreal[i]<-gCal*mL[i-1]
        gkcareal[i]<-gKca*mkca[i-1]
        
        iNaLeak[i] <- gNaleak*(Vnet[i-1]-ENaleak)        
        iKLeak[i] <- gKleak*(Vnet[i-1]-EKleak)        
        iNa[i]<- gna*h[i-1]*(Vnet[i-1]-Ena)*m[i-1]^3
        iK[i] <- gk*(Vnet[i-1]-Ek)*n[i-1]^4
        iCl[i] <- gCl*(Vnet[i-1]-Ecl)*m[i-1]
        iKca[i] <- gKca*(Vnet[i-1]-Ek)*mkca[i-1]       
        iA[i] <- gA*(Vnet[i-1]-Ek)*(0.6*m1A[i-1]^4*h1A[i-1]+0.4*m2A[i-1]^4*h2A[i-1])
        iAHP[i] <- gAHP*(Vnet[i-1]-Ek)*mAHP[i-1]^2
        iM[i] <- gM*(Vnet[i-1]-Ek)*mM[i-1]        
        iT[i] <- ((gT*(mT[i-1]^2)*hT[i-1]*(2^2)*(Vnet[i-1])*Faraday^2)/(R*Temp)) * ( InCa-OutCa)* (exp((-2*Faraday*Vnet[i-1])/(R*Temp))/(1-exp((-2*Faraday*Vnet[i-1])/(R*Temp))))
        iCal[i] <- ((gCal*(mL[i-1]^2)*(2^2)*(Vnet[i-1])*Faraday^2)/(R*Temp)) * ( InCa-OutCa)* (exp((-2*Faraday*Vnet[i-1])/(R*Temp))/(1-exp((-2*Faraday*Vnet[i-1])/(R*Temp))))
       
        #####  change here
        
        Vnet[i] <- Vnet[i-1]+((Inet[i]-iNaLeak[i]-iKLeak[i]-iNa[i]-iK[i]-iCl[i]-iCal[i]-iKca[i]-iT[i]-iA[i]-iAHP[i]-iM[i])/C)*dt
        
        m[i] <- m[i-1]+(am(Vnet[i-1])*(1-m[i-1])-bm(Vnet[i-1])*m[i-1])*dt
        h[i] <- h[i-1]+(ah(Vnet[i-1])*(1-h[i-1])-bh(Vnet[i-1])*h[i-1])*dt
        n[i] <- n[i-1]+(an(Vnet[i-1])*(1-n[i-1])-bn(Vnet[i-1])*n[i-1])*dt 
        mAHP[i] <- mAHP[i-1]+(amAHP*(1-mAHP[i-1])-bmAHP*mAHP[i-1])*dt
        mkca[i] <- mkca[i-1]+(amkca(Vnet[i-1])*(1-mkca[i-1])-bmkca(Vnet[i-1])*mkca[i-1])*dt
        mM[i] <- mM[i-1]+((minfM(Vnet[i-1])-mM[i-1])/mtauM(Vnet[i-1]))*dt        
        m1A[i] <- m1A[i-1]+((m1infA(Vnet[i-1])-m1A[i-1])/m1tauA(Vnet[i-1]))*dt
        h1A[i] <- h1A[i-1]+((h1infA(Vnet[i-1])-h1A[i-1])/h1tauA(Vnet[i-1]))*dt
        m2A[i] <- m2A[i-1]+((m2infA(Vnet[i-1])-m2A[i-1])/m2tauA(Vnet[i-1]))*dt
        h2A[i] <- h2A[i-1]+((h2infA(Vnet[i-1])-h2A[i-1])/h2tauA(Vnet[i-1]))*dt
        
        mT[i] <- mT[i-1]+((minfT(Vnet[i-1])-mT[i-1])/mtauT(Vnet[i-1]))*dt
        hT[i] <- hT[i-1]+((hinfT(Vnet[i-1])-hT[i-1])/htauT(Vnet[i-1]))*dt
        
        mL[i] <- mL[i-1]+(amCal(Vnet[i-1])*(1-mL[i-1])-bmCal(Vnet[i-1])*mL[i-1])*dt
        
        # change here 
        
        
        
      }
      
      
      
      return(data.frame(realTime,times,Vnet,Inet,iKLeak,iNaLeak,iNa,iK,iCl,iCal,iKca,iA,iAHP,iT,iM,gKleak,gNaleak,gnareal,gkreal,gclreal,gahpreal,gareal,gcalreal,gtreal,gmreal,gkcareal))
    }
    
    
    if(input$whatClamp=="voltage"){
   
      
      TBase=input$BaselineDuration; TClamp=input$ClampDuration; TEnd=input$EndDuration; dt=input$dt;
      
      times = seq(from=0, to = TClamp+TEnd+TBase, by = dt)
      realTime = vector(mode="integer",length=length(times))      
      
      Vnet=vector(mode="integer",length=length(times))
      m=vector(mode="integer",length=length(times))
      h=vector(mode="integer",length=length(times))
      n=vector(mode="integer",length=length(times))
      mkca=vector(mode="integer",length=length(times))
      mAHP=vector(mode="integer",length=length(times))
      mM=vector(mode="integer",length=length(times))
      m1A= vector(mode="integer",length=length(times))
      h1A= vector(mode="integer",length=length(times))
      m2A= vector(mode="integer",length=length(times))
      h2A= vector(mode="integer",length=length(times))
      mT= vector(mode="integer",length=length(times))
      hT= vector(mode="integer",length=length(times))
      mL= vector(mode="integer",length=length(times))
      
      ### change here     
      iKLeak=vector(mode="integer",length=length(times))      
      iNaLeak=vector(mode="integer",length=length(times))            
      iNa=vector(mode="integer",length=length(times))
      iK=vector(mode="integer",length=length(times))
      iCl=vector(mode="integer",length=length(times))
      iCal=vector(mode="integer",length=length(times))
      iKca=vector(mode="integer",length=length(times))
      iA=vector(mode="integer",length=length(times))
      iT=vector(mode="integer",length=length(times))
      iAHP=vector(mode="integer",length=length(times))
      iM=vector(mode="integer",length=length(times))
      Inet=vector(mode="integer",length=length(times))
      
      gNaleak=vector(mode="integer",length=length(times))            
      gKleak=vector(mode="integer",length=length(times))                  
      gnareal=vector(mode="integer",length=length(times))
      gkreal=vector(mode="integer",length=length(times))
      gclreal=vector(mode="integer",length=length(times))
      gahpreal=vector(mode="integer",length=length(times))
      gkcareal=vector(mode="integer",length=length(times))
      gareal=vector(mode="integer",length=length(times))
      gtreal=vector(mode="integer",length=length(times))
      gmreal=vector(mode="integer",length=length(times))
      gcalreal=vector(mode="integer",length=length(times))
      
      
      #Ena=input$Ena; Ek=input$Ek; Ec=input$Ec;
      OutNa=input$OutNa
      OutK=input$OutK
      OutCl=input$OutCl
      OutCa=input$OutCa
      InNa=input$InNa
      InK=input$InK
      InCl=input$InCl
      InCa=input$InCa
      
      R=8314
      Temp=273.16+25
      Faraday=9.648*10^4
      
      Ena=((8314*(273.16+25))/(1*9.648*10^4))*log(OutNa/InNa) 
      Ek=((8314*(273.16+25))/(1*9.648*10^4))*log(OutK/InK) 
      Ecl=((8314*(273.16+25))/(-1*9.648*10^4))*log(OutCl/InCl) 
      Eca=((8314*(273.16+25))/(-1*9.648*10^4))*log(OutCa/InCa) 
      EKleak=-105
      ENaleak=45
      
      gNaleak=input$GNaleak;gKleak=input$GKleak;gna=input$Gna; gk=input$Gk; gCl=input$Gcl;gCal=input$Gcal;gKca=input$Gkca;gA=input$Ga;gAHP=input$Gahp;gT=input$Gt;gM=input$Gm;
      C=input$C ; 
      
      
      Vnet[1]=(gk*Ek+gna*Ena+gCl*Ecl+gCal*Eca+gKca*Ek+gT*Eca+gA*Ek+gAHP*Ek+gM*Ek+gKleak*EKleak+gNaleak*ENaleak)/(gna+gk+gCl+gCal+gKca+gA+gAHP+gT+gM+gKleak+gNaleak);
      realTime[1]=0
      
      m[1]=0.052 ;h[1]=0.596; n[1]=0.317;mAHP[1]=0.1;mkca[1]=0.1;mM[1]=0.1; m1A[1]=0.1;h1A[1]=0.1;m2A[1]=0.1;h2A[1]=0.1;mT[1]=0.01;hT[1]=0.01;mL[1]=0.01; Inet[1]=0;
      ## change here
      
      am <- function(v) 0.1*(v+40)/(1-exp(-(v+40)/10))
      bm <- function(v) 4*exp(-(v+65)/18)
      ah <- function(v) 0.07*exp(-(v+65)/20)
      bh <- function(v) 1/(1+exp(-(v+35)/10))
      an <- function(v) 0.01*(v+55)/(1-exp(-(v+55)/10))
      bn <- function(v) 0.125*exp(-(v+65)/80)
      amAHP = (1.2*10^9)*(InCa)^2      
      bmAHP = 0.01 
      amkca <- function(v) (2.5*10^5)*(InCa)*exp(v/24)
      bmkca <- function(v) 0.1*exp(-v/24)
      minfM <- function(v) 1/(1+exp((-v+35)/10))
      mtauM <- function(v) 1000 / (3.3*exp((v+35)/20)+exp((-v+35)/20))
      
      m1infA <- function(v) 1/(1+exp((v+60)/-8.5))
      m1tauA <- function(v) 1/ (exp((v+35.82)/19.69)+exp((v+79.69)/-12.7)+0.37)
      h1infA <- function(v) 1/(1+exp((v+78)/6))
      h1tauA <- function(v) { if (v< -63) { return ( 1/(exp((v+46.05)/5)+exp((v+238.4)/-37.45)))  } else { return(19) }}
      
      m2infA <- function(v) 1/(1+exp((v+36)/-20))
      m2tauA <- function(v) 1/ (exp((v+35.82)/19.69)+exp((v+79.69)/-127)+0.37)
      h2infA <- function(v) 1/(1+exp((v+78)/6))
      h2tauA <- function(v) { if (v< -73) { return ( 1/(exp((v+46.05)/5)+exp((v+238.4)/-37.45)))  } else { return(60) }} 
      
      minfT <- function(v) 1/(1+exp((v+60.5)/-6.2))
      mtauT <- function(v) 1/ (exp((v+131.6)/-16.7)+exp((v+16.8)/18.2)+0.612)
      hinfT <- function(v) 1/(1+exp((v+84.5)/4.03))
      htauT <- function(v) { if (v< -80) { return ( exp((v+467)/66.6))  } else { return(  exp((v+21.88)/-10.52)+28 ) }}
      
      amCal <- function(v) 1.6/(1+exp(-0.072*(v+5)))
      bmCal <- function(v) 0.02*(v-1.3)/(exp((v-1.31)/5.36)-1)
      
      # change here
      
      
      for(i in seq(2,length(times))){            
        if(times[i]>TBase & times[i]<(TBase+TClamp)){
          Vnet[i] <-  input$Vapplied
          
        }
        else{ Vnet[i] <- Vnet[1] }
        
        realTime[i] <- dt *i 
        gnareal[i]<-gna*h[i-1]*m[i-1]^3
        gkreal[i]<-gk*n[i-1]^4
        gclreal[i]<-gCl*m[i-1]
        gahpreal[i]<-gAHP*mAHP[i-1]^2
        gareal[i]<-gA*(0.6*m1A[i-1]^4*h1A[i-1]+0.4*m2A[i-1]^4*h2A[i-1])
        gtreal[i]<-gT*mT[i-1]
        gmreal[i]<-gM*mM[i-1]
        gcalreal[i]<-gCal*mL[i-1]
        gkcareal[i]<-gKca*mkca[i-1]
        
        iKLeak[i] <- gKleak*(Vnet[i-1]-EKleak)              
        iNaLeak[i] <- gNaleak*(Vnet[i-1]-ENaleak)                      
        iNa[i]<- gna*h[i-1]*(Vnet[i-1]-Ena)*m[i-1]^3
        iK[i] <- gk*(Vnet[i-1]-Ek)*n[i-1]^4
        iCl[i] <- gCl*(Vnet[i-1]-Ecl)*m[i-1]
        iKca[i] <- gKca*(Vnet[i-1]-Ek)*mkca[i-1]       
        iA[i] <- gA*(Vnet[i-1]-Ek)*(0.6*m1A[i-1]^4*h1A[i-1]+0.4*m2A[i-1]^4*h2A[i-1])
        iAHP[i] <- gAHP*(Vnet[i-1]-Ek)*mAHP[i-1]^2
        iM[i] <- gM*(Vnet[i-1]-Ek)*mM[i-1]        
        iT[i] <- ((gT*(mT[i-1]^2)*hT[i-1]*(2^2)*(Vnet[i-1])*Faraday^2)/(R*Temp)) * ( InCa-OutCa)* (exp((-2*Faraday*Vnet[i-1])/(R*Temp))/(1-exp((-2*Faraday*Vnet[i-1])/(R*Temp))))
        iCal[i] <- ((gCal*(mL[i-1]^2)*(2^2)*(Vnet[i-1])*Faraday^2)/(R*Temp)) * ( InCa-OutCa)* (exp((-2*Faraday*Vnet[i-1])/(R*Temp))/(1-exp((-2*Faraday*Vnet[i-1])/(R*Temp))))
        
        
        #####  change here
        
        Inet[i] <- (-iKLeak[i]-iKLeak[i]-iNa[i]-iK[i]-iCl[i]-iCal[i]-iKca[i]-iT[i]-iA[i]-iAHP[i]-iM[i])
        
        m[i] <- m[i-1]+(am(Vnet[i-1])*(1-m[i-1])-bm(Vnet[i-1])*m[i-1])*dt
        h[i] <- h[i-1]+(ah(Vnet[i-1])*(1-h[i-1])-bh(Vnet[i-1])*h[i-1])*dt
        n[i] <- n[i-1]+(an(Vnet[i-1])*(1-n[i-1])-bn(Vnet[i-1])*n[i-1])*dt 
        mAHP[i] <- mAHP[i-1]+(amAHP*(1-mAHP[i-1])-bmAHP*mAHP[i-1])*dt
        mkca[i] <- mkca[i-1]+(amkca(Vnet[i-1])*(1-mkca[i-1])-bmkca(Vnet[i-1])*mkca[i-1])*dt
        mM[i] <- mM[i-1]+((minfM(Vnet[i-1])-mM[i-1])/mtauM(Vnet[i-1]))*dt        
        m1A[i] <- m1A[i-1]+((m1infA(Vnet[i-1])-m1A[i-1])/m1tauA(Vnet[i-1]))*dt
        h1A[i] <- h1A[i-1]+((h1infA(Vnet[i-1])-h1A[i-1])/h1tauA(Vnet[i-1]))*dt
        m2A[i] <- m2A[i-1]+((m2infA(Vnet[i-1])-m2A[i-1])/m2tauA(Vnet[i-1]))*dt
        h2A[i] <- h2A[i-1]+((h2infA(Vnet[i-1])-h2A[i-1])/h2tauA(Vnet[i-1]))*dt
        
        mT[i] <- mT[i-1]+((minfT(Vnet[i-1])-mT[i-1])/mtauT(Vnet[i-1]))*dt
        hT[i] <- hT[i-1]+((hinfT(Vnet[i-1])-hT[i-1])/htauT(Vnet[i-1]))*dt
        
        mL[i] <- mL[i-1]+(amCal(Vnet[i-1])*(1-mL[i-1])-bmCal(Vnet[i-1])*mL[i-1])*dt
        
        # change here 
        
        
        
      }
      
      
      
      return(data.frame(realTime,times,Vnet,Inet,iKLeak,iNaLeak,iNa,iK,iCl,iCal,iKca,iA,iAHP,iT,iM,gKleak,gNaleak,gnareal,gkreal,gclreal,gahpreal,gareal,gcalreal,gtreal,gmreal,gkcareal))
    }
    
      
    
  })
       
  output$downloadData <- downloadHandler(
    
    
    filename = function() {
      paste('data-', Sys.Date(), 'csv', sep=".")
    },
    content = function(con) {
      write.csv(data.frame(myOutput()$realTime,myOutput()$Vnet,myOutput()$Inet,myOutput()$iNa,myOutput()$iK,myOutput()$iCl,myOutput()$iKca,myOutput()$iM,myOutput()$iAHP,myOutput()$iA,myOutput()$iCal,myOutput()$iT), con)
    }
    
  )
        
  
  
  
  output$VnetPlot <- renderPlot({   
    
    plot(myOutput()$times,myOutput()$Vnet,main="VOLTAGE", col="blue",
         xlab="time",ylab="voltage",type="l")        
    
  })
  
  
  
  output$allIPlot <- renderPlot({   
    
    
    plot(myOutput()$times,myOutput()$Inet,main="CURRENT", col="#000000",type="l",
         xlab="time",ylab="current", ylim=c(min(myOutput()$iNa,myOutput()$iK,myOutput()$iCl,myOutput()$iKca,myOutput()$iM,myOutput()$iAHP,myOutput()$iA,myOutput()$iCal,myOutput()$iT), max(myOutput()$iNa,myOutput()$iK,myOutput()$iCl,myOutput()$iKca,myOutput()$iM,myOutput()$iAHP,myOutput()$iA,myOutput()$iCal,myOutput()$iT))) 
    
    lines(myOutput()$times,myOutput()$iNa,col="#00FF00")
    lines(myOutput()$times,myOutput()$iK,col="#0000FF")
    lines(myOutput()$times,myOutput()$iCl,col="#FF0000")
    lines(myOutput()$times,myOutput()$iAHP,col="#FF00FF")
    lines(myOutput()$times,myOutput()$iKca,col="#00FFFF")
    lines(myOutput()$times,myOutput()$iA,col="#FFFF00")
    lines(myOutput()$times,myOutput()$iT,col="#006600")
    lines(myOutput()$times,myOutput()$iM,col="#660000")
    lines(myOutput()$times,myOutput()$iCal,col="#000066")
    lines(myOutput()$times,myOutput()$iKLeak,col="#660066")
    lines(myOutput()$times,myOutput()$iNaLeak,col="#666600")
    
    
    
  })
  
  
  
  
  output$allGPlot <- renderPlot({   
    
    
    plot(myOutput()$times,myOutput()$gnareal,main="CONDUCTANCE", col="#00FF00",type="l",
         xlab="time",ylab="conductance", ylim=c(min(myOutput()$gnareal,myOutput()$gkreal,myOutput()$gclreal,myOutput()$gahpreal,myOutput()$gkcareal,myOutput()$gareal,myOutput()$gtreal,myOutput()$gmreal,myOutput()$gcalreal,myOutput()$gleakreal), max(myOutput()$gnareal,myOutput()$gkreal,myOutput()$gclreal,myOutput()$gahpreal,myOutput()$gkcareal,myOutput()$gareal,myOutput()$gtreal,myOutput()$gmreal,myOutput()$gcalreal,myOutput()$gleakreal))) 
    
    lines(myOutput()$times,myOutput()$gkreal,col="#0000FF")
    lines(myOutput()$times,myOutput()$gclreal,col="#FF0000")
    lines(myOutput()$times,myOutput()$gahpreal,col="#FF00FF")
    lines(myOutput()$times,myOutput()$gkcareal,col="#00FFFF")
    lines(myOutput()$times,myOutput()$gareal,col="#FFFF00")
    lines(myOutput()$times,myOutput()$gtreal,col="#006600")
    lines(myOutput()$times,myOutput()$gmreal,col="#660000")
    lines(myOutput()$times,myOutput()$gcalreal,col="#000066")
    lines(myOutput()$times,myOutput()$gKleak,col="#660066")
    lines(myOutput()$times,myOutput()$gNaleak,col="#666600")
    
    
    
  })
  
  
  
  
})

