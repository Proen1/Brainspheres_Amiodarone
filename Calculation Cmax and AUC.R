# This code is for TK simulations of amiodrone in the concentrations used for making the cytotxicity dose-response curves
#for the trapezoid method it needs the following package
library(pracma)

#optimized values from before
Ka_plastic= 0.412
Kd_plastic= 0.289
Kdeg=  0.005
Ka_cells=  0.406
Kd_cells=  0.314

Vol_med=0.002       # in L

#Experimental dose-response curves

Nom_C_1d=c(0,1.25,2.5,3.75,5, 10,15)
Resp_1d=c(100.000,83.699,77.918,50.302,24.993,-1.398,-4.918)

Nom_C_2d=c(0, 1.25, 2.5,3.75,5,10,15)
Resp_2d=c(1.000,0.857, 0.826,0.546,0.123, -0.023,-0.027)*100

Nom_C_7d=c(0,0.625, 1.25,1.9,2.5,5,10)  
Resp_7d=c(1.000,1.098,1.011,0.343,0.158,-0.035,-0.031)*100

Nom_C_14d=c(0,0.625,1.25,1.9,2.5, 5) 
Resp_14d=c(1.000,0.877, 1.146,0.260,-0.013,-0.110)  *100        

len_columns=length(Nom_C_1d)+length(Nom_C_2d)+length(Nom_C_7d)+length(Nom_C_14d)
Curve_DR=data.frame(
  "Nominal_C"=c(Nom_C_1d,Nom_C_2d,Nom_C_7d,Nom_C_14d),
  "Viability"=c(Resp_1d,Resp_2d,Resp_7d,Resp_14d),
  "Exp_length"=c(rep("1_day",length(Nom_C_1d)),rep("2_days",length(Nom_C_2d)),rep("7_days",length(Nom_C_7d)),rep("14_days",length(Nom_C_14d))),
  "Cmax_medium"=rep(0,length.out=len_columns),
  "Cmax_cells"=rep(0,length.out=len_columns),
  "AUC_medium"=rep(0,length.out=len_columns),
  "AUC_cells"=rep(0,length.out=len_columns)
)

# Make cycle to calculate for each concentration
for (i in 1:(nrow(Curve_DR))){
  
  Dose_conc=Curve_DR$Nominal_C[i]*1000 # nMol
  Dose<-Dose_conc*Vol_med
  
  # Make repeated exposure simulations
  
  #First 48 h
  times<-seq(0,48,by=1/60) 
  state<-c("Amedium"=Dose,"Aplastic"=0,"Acells"=0)
  TK_1<-ode(y=state,times=times,func=TK_model_2,
            parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Ka_cells=Ka_cells,Kd_cells=Kd_cells ))
  
  #Next 72 h
  times<-seq(48,120,by=1/60) 
  state<-c("Amedium"=Dose,"Aplastic"=as.numeric(TK_1[48*60,"Aplastic"]),"Acells"=as.numeric(TK_1[(48)*60,"Acells"]))
  TK_2<-ode(y=state,times=times,func=TK_model_2,
            parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Ka_cells=Ka_cells,Kd_cells=Kd_cells))
  
  #Next 48 h 
  times<-seq(120,168,by=1/60) 
  state<-c("Amedium"=Dose,"Aplastic"=as.numeric(TK_2[(120-48)*60,"Aplastic"]),"Acells"=as.numeric(TK_2[(120-48)*60,"Acells"]))
  TK_3<-ode(y=state,times=times,func=TK_model_2,
            parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Ka_cells=Ka_cells,Kd_cells=Kd_cells))
  
  #Withdrawn period
  #Next 48 h, complete medium change, no AMI
  times<-seq(168,216,by=1/60) 
  state<-c("Amedium"=0,"Aplastic"=as.numeric(TK_3[(168-120)*60,"Aplastic"]),"Acells"=as.numeric(TK_3[(168-120)*60,"Acells"]))
  TK_4<-ode(y=state,times=times,func=TK_model_2,
            parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Ka_cells=Ka_cells,Kd_cells=Kd_cells))
  
  #Next 72 h, 25%medium changed, no AMI
  times<-seq(216,288,by=1/60) 
  state<-c("Amedium"=as.numeric(TK_4[(216-168)*60,"Amedium"])*3/4,"Aplastic"=as.numeric(TK_4[(216-168)*60,"Aplastic"]),"Acells"=as.numeric(TK_4[(216-168)*60,"Acells"]))
  TK_5<-ode(y=state,times=times,func=TK_model_2,
            parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Ka_cells=Ka_cells,Kd_cells=Kd_cells))
  
  #Next 48 h, 25%medium changed, no AMI
  times<-seq(288,336,by=1/60) 
  state<-c("Amedium"=as.numeric(TK_5[(288-216)*60,"Amedium"])*3/4,"Aplastic"=as.numeric(TK_5[(288-216)*60,"Aplastic"]),"Acells"=as.numeric(TK_5[(288-216)*60,"Acells"]))
  TK_6<-ode(y=state,times=times,func=TK_model_2,
            parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Ka_cells=Ka_cells,Kd_cells=Kd_cells))
  
  #Calculate Cmax and AUC depending on the exposure length
  if  (Curve_DR$Exp_length[i]=="1_day"){
    Curve_DR$Cmax_medium[i]=max(c(TK_1[seq(1,(24+1)*60,by=1),"Amedium"]))/Vol_med
    Curve_DR$Cmax_cells[i]=max(c(TK_1[seq(1,(24+1)*60,by=1),"Acells"]))/20 #per 100,000 cell
    
    Curve_DR$AUC_medium[i]=trapz(TK_1[seq(1,(24+1)*60,by=1),1],TK_1[seq(1,(24+1)*60,by=1),"Amedium"])
    Curve_DR$AUC_cells[i]=trapz(TK_1[seq(1,(24+1)*60,by=1),1],TK_1[seq(1,(24+1)*60,by=1),"Acells"])
    
  } else if  (Curve_DR$Exp_length[i]=="2_days"){
    # Calculate Cmax
    Curve_DR$Cmax_medium[i]=max(c(TK_1[,"Amedium"]))/Vol_med
    Curve_DR$Cmax_cells[i]=max(c(TK_1[,"Acells"]))/20 #per 100,000 cell
    
    Curve_DR$AUC_medium[i]=
      trapz(TK_1[,1],TK_1[,"Amedium"]) 
    Curve_DR$AUC_cells[i]=
      trapz(TK_1[,1],TK_1[,"Acells"]) 
    
  } else if (Curve_DR$Exp_length[i]=="7_days"){
    
    Curve_DR$Cmax_medium[i]=max(c(TK_1[,"Amedium"],TK_2[,"Amedium"],TK_3[,"Amedium"]))/Vol_med
    Curve_DR$Cmax_cells[i]=max(c(TK_1[,"Acells"],TK_2[,"Acells"],TK_3[,"Acells"]))/20 #per 100,000 cell
    Curve_DR$AUC_medium[i]=
      trapz(TK_1[,1],TK_1[,"Amedium"]) +
      trapz(TK_2[,1],TK_2[,"Amedium"]) +
      trapz(TK_3[,1],TK_3[,"Amedium"]) 
    
    Curve_DR$AUC_cells[i]=
      trapz(TK_1[,1],TK_1[,"Acells"]) +
      trapz(TK_2[,1],TK_2[,"Acells"]) +
      trapz(TK_3[,1],TK_3[,"Acells"]) 
    
  }else{
    
    # Calculate Cmax
    Curve_DR$Cmax_medium[i]=max(c(TK_1[,"Amedium"],TK_2[,"Amedium"],TK_3[,"Amedium"],TK_4[,"Amedium"],TK_5[,"Amedium"],TK_6[,"Amedium"]))/Vol_med
    Curve_DR$Cmax_cells[i]=max(c(TK_1[,"Acells"],TK_2[,"Acells"],TK_3[,"Acells"],TK_4[,"Acells"],TK_5[,"Acells"],TK_6[,"Acells"]))/20 #per 100,000 cell
    
    #Calculate Area under the curve
    Curve_DR$AUC_medium[i]=
      Curve_DR$AUC_medium[i]=
      trapz(TK_1[,1],TK_1[,"Amedium"]) +
      trapz(TK_2[,1],TK_2[,"Amedium"]) +
      trapz(TK_3[,1],TK_3[,"Amedium"]) +
      trapz(TK_1[,1],TK_4[,"Amedium"]) +
      trapz(TK_2[,1],TK_5[,"Amedium"]) +
      trapz(TK_3[,1],TK_6[,"Amedium"]) 
    
    Curve_DR$AUC_cells[i]=
      trapz(TK_1[,1],TK_1[,"Acells"]) +
      trapz(TK_2[,1],TK_2[,"Acells"]) +
      trapz(TK_3[,1],TK_3[,"Acells"]) +
      trapz(TK_1[,1],TK_4[,"Acells"]) +
      trapz(TK_2[,1],TK_5[,"Acells"]) +
      trapz(TK_3[,1],TK_6[,"Acells"]) 
  }
}


write.csv(Curve_DR,"Curve_DR.csv")