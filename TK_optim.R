#This code is to perform optimization of amiodarone TK model

#Description of the error function
#Define Error function----------------------------------------------------------
vallerror<-function(initialValues) {
  Ka_plastic=initialValues[1]
  Kd_plastic=initialValues[2]
  Kdeg=initialValues[3]
  Ka_cells=initialValues[4]
  Kd_cells=initialValues[5]
  
  
  #Simulation control wells
  ##Simulations for repeated 1 uM
  Dose<-0.001516*1000
  #First48 h, dosing 1 uM
  times<-seq(0,48,by=1/60)
  state<-c("Amedium"=Dose,"Aplastic"=0,"Adeg"=0)
  Ctr_TK_1_1uM<-ode(y=state,times=times,func=TK_model_1,
                    parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 72 h, dosing 1 uM
  times<-seq(48,120,by=1/60)
  state<-c("Amedium"=Dose,"Aplastic"=as.numeric(Ctr_TK_1_1uM[48*60+1,"Aplastic"]),"Adeg"=0)
  Ctr_TK_2_1uM<-ode(y=state,times=times,func=TK_model_1,
                    parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 48 h, dosing 1 uM
  times<-seq(120,168,by=1/60)
  state<-c("Amedium"=Dose,"Aplastic"=as.numeric(Ctr_TK_2_1uM[((120-48)*60+1),"Aplastic"]),"Adeg"=0)
  Ctr_TK_3_1uM<-ode(y=state,times=times,func=TK_model_1,
                    parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Withdrawn period
  #Next 48 h, complete medium change, no AMI
  times<-seq(168,216,by=1/60)
  state<-c("Amedium"=0,"Aplastic"=as.numeric(Ctr_TK_3_1uM[((168-120)*60+1),"Aplastic"]),"Adeg"=0)
  Ctr_TK_4_1uM<-ode(y=state,times=times,func=TK_model_1,
                    parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 72 h, 25%medium changed, no AMI
  times<-seq(216,288,by=1/60)
  state<-c("Amedium"=as.numeric(Ctr_TK_4_1uM[((216-168)*60+1),"Amedium"])*3/4,"Aplastic"=as.numeric(Ctr_TK_4_1uM[((216-168)*60+1),"Aplastic"]),"Adeg"=0)
  Ctr_TK_5_1uM<-ode(y=state,times=times,func=TK_model_1,
                    parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 48 h, 25%medium changed, no AMI
  times<-seq(288,336,by=1/60)
  state<-c("Amedium"=as.numeric(Ctr_TK_5_1uM[((288-216)*60+1),"Amedium"])*3/4,"Aplastic"=as.numeric(Ctr_TK_5_1uM[((288-216)*60+1),"Aplastic"]),"Adeg"=0)
  Ctr_TK_6_1uM<-ode(y=state,times=times,func=TK_model_1,
                    parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  ##Simulations for repeated 2 uM-----------------------------------------------
  
  Dose<- 0.002952*1000
  #First48 h, dosing 2 uM
  times<-seq(0,48,by=1/60)
  state<-c("Amedium"=Dose,"Aplastic"=0,"Adeg"=0)
  Ctr_TK_1_2uM<-ode(y=state,times=times,func=TK_model_1,
                    parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 72 h, dosing 2 uM
  times<-seq(48,120,by=1/60)
  state<-c("Amedium"=Dose,"Aplastic"=as.numeric(Ctr_TK_1_1uM[48*60+1,"Aplastic"]),"Adeg"=0)
  Ctr_TK_2_2uM<-ode(y=state,times=times,func=TK_model_1,
                    parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 48 h, dosing 2 uM
  times<-seq(120,168,by=1/60)
  state<-c("Amedium"=Dose,"Aplastic"=as.numeric(Ctr_TK_2_1uM[((120-48)*60+1),"Aplastic"]),"Adeg"=0)
  Ctr_TK_3_2uM<-ode(y=state,times=times,func=TK_model_1,
                    parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Withdrawn period
  #Next 48 h, complete medium change, no AMI
  times<-seq(168,216,by=1/60)
  state<-c("Amedium"=0,"Aplastic"=as.numeric(Ctr_TK_3_1uM[((168-120)*60+1),"Aplastic"]),"Adeg"=0)
  Ctr_TK_4_2uM<-ode(y=state,times=times,func=TK_model_1,
                    parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 72 h, 25%medium changed, no AMI
  times<-seq(216,288,by=1/60)
  state<-c("Amedium"=as.numeric(Ctr_TK_4_1uM[((216-168)*60+1),"Amedium"])*3/4,"Aplastic"=as.numeric(Ctr_TK_4_1uM[((216-168)*60+1),"Aplastic"]),"Adeg"=0)
  Ctr_TK_5_2uM<-ode(y=state,times=times,func=TK_model_1,
                    parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 48 h, 25%medium changed, no AMI
  times<-seq(288,336,by=1/60)
  state<-c("Amedium"=as.numeric(Ctr_TK_5_1uM[((288-216)*60+1),"Amedium"])*3/4,"Aplastic"=as.numeric(Ctr_TK_5_1uM[((288-216)*60+1),"Aplastic"]),"Adeg"=0)
  Ctr_TK_6_2uM<-ode(y=state,times=times,func=TK_model_1,
                    parms=c(Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  
  #Simulations acute 3 uM
  
  Ctr_TK_optim_3uM<-ode(y=c("Amedium"=Dose_3uM,"Aplastic"=0,"Adeg"=0),times=seq(0,48,by=1/60),func=TK_model_1,
                        parms=c(Ka_plastic=Ka_plastic,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Simulation well with cells
  #First48 h, dosing 1 uM
  times<-seq(0,48,by=1/60)
  state<-c("Amedium"=Dose_1uM,"Aplastic"=0, "Acells"=0)
  TK_1_1uM<-ode(y=state,times=times,func=TK_model_2,
                parms=c(Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic ,Kdeg=Kdeg))
  
  #Next 72 h, dosing 1 uM
  times<-seq(48,120,by=1/60)
  state<-c("Amedium"=Dose_1uM,"Aplastic"=as.numeric(TK_1_1uM[48*60+1,"Aplastic"]), "Acells"=as.numeric(TK_1_1uM[48*60+1,"Acells"]))
  TK_2_1uM<-ode(y=state,times=times,func=TK_model_2,
                parms=c(Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 48 h, dosing 1 uM
  times<-seq(120,168,by=1/60)
  state<-c("Amedium"=Dose_1uM,"Aplastic"=as.numeric(TK_2_1uM[((120-48)*60+1),"Aplastic"]),"Acells"=as.numeric(TK_2_1uM[((120-48)*60+1),"Acells"]))
  TK_3_1uM<-ode(y=state,times=times,func=TK_model_2,
                parms=c(Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Withdrawn period
  #Next 48 h, complete medium change, no AMI
  times<-seq(168,216,by=1/60)
  state<-c("Amedium"=0,"Aplastic"=as.numeric(TK_3_1uM[((168-120)*60+1),"Aplastic"]),"Acells"=as.numeric(TK_3_1uM[((168-120)*60+1),"Acells"]))
  TK_4_1uM<-ode(y=state,times=times,func=TK_model_2,
                parms=c(Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 72 h, 25%medium changed, no AMI
  times<-seq(216,288,by=1/60)
  state<-c("Amedium"=as.numeric(TK_4_1uM[((216-168)*60+1),"Amedium"])*3/4,"Aplastic"=as.numeric(TK_4_1uM[((216-168)*60+1),"Aplastic"]),"Acells"=as.numeric(TK_4_1uM[((216-168)*60+1),"Acells"]))
  TK_5_1uM<-ode(y=state,times=times,func=TK_model_2,
                parms=c(Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 48 h, 25%medium changed, no AMI
  times<-seq(288,336,by=1/60)
  state<-c("Amedium"=as.numeric(TK_5_1uM[(288-216)*60+1,"Amedium"])*3/4,"Aplastic"=as.numeric(TK_5_1uM[((288-216)*60+1),"Aplastic"]),"Acells"=as.numeric(TK_5_1uM[((288-216)*60+1),"Acells"]))
  TK_6_1uM<-ode(y=state,times=times,func=TK_model_2,
                parms=c(Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  ##Simulations for repeated 2 uM-----------------------------------------------
  
  
  #First48 h, dosing 2 uM
  times<-seq(0,48,by=1/60)
  state<-c("Amedium"=Dose_2uM,"Aplastic"=0,"Acells"=0)
  TK_1_2uM<-ode(y=state,times=times,func=TK_model_2,
                parms=c(Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 72 h, dosing 2 uM
  times<-seq(48,120,by=1/60)
  state<-c("Amedium"=Dose_2uM,"Aplastic"=as.numeric(TK_1_1uM[48*60+1,"Aplastic"]),"Acells"=as.numeric(TK_1_1uM[48,"Acells"]))
  TK_2_2uM<-ode(y=state,times=times,func=TK_model_2,
                parms=c(Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 48 h, dosing 2 uM
  times<-seq(120,168,by=1/60)
  state<-c("Amedium"=Dose_2uM,"Aplastic"=as.numeric(TK_2_1uM[((120-48)*60+1),"Aplastic"]),"Acells"=as.numeric(TK_2_1uM[((120-48)*60+1),"Acells"]))
  TK_3_2uM<-ode(y=state,times=times,func=TK_model_2,
                parms=c(Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Withdrawn period
  #Next 48 h, complete medium change, no AMI
  times<-seq(168,216,by=1/60)
  state<-c("Amedium"=0,"Aplastic"=as.numeric(TK_3_1uM[((168-120)*60+1),"Aplastic"]),"Acells"=as.numeric(TK_3_1uM[((168-120)*60+1),"Acells"]))
  TK_4_2uM<-ode(y=state,times=times,func=TK_model_2,
                parms=c(Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 72 h, 25%medium changed, no AMI
  times<-seq(216,288,by=1/60)
  state<-c("Amedium"=as.numeric(TK_4_1uM[((216-168)*60+1),"Amedium"])*3/4,"Aplastic"=as.numeric(TK_4_1uM[((216-168)*60+1),"Aplastic"]),"Acells"=as.numeric(TK_4_1uM[((216-168)*60+1),"Acells"]))
  TK_5_2uM<-ode(y=state,times=times,func=TK_model_2,
                parms=c(Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #Next 48 h, 25%medium changed, no AMI
  times<-seq(288,336,by=1/60)
  state<-c("Amedium"=as.numeric(TK_5_1uM[((288-216)*60+1),"Amedium"])*3/4,"Aplastic"=as.numeric(TK_5_1uM[((288-216)*60+1),"Aplastic"]),"Acells"=as.numeric(TK_5_1uM[((288-216)*60+1),"Acells"]))
  TK_6_2uM<-ode(y=state,times=times,func=TK_model_2,
                parms=c(Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_plastic=Ka_plastic ,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  
  #Simulations acute 3 uM
  
  TK_optim_3uM<-ode(y=c("Amedium"=Dose_3uM,"Aplastic"=0,"Acells"=0),times=seq(0,48,by=1/60),func=TK_model_2,
                    parms=c(Ka_cells=Ka_cells,Kd_cells=Kd_cells,Ka_plastic=Ka_plastic,Kd_plastic=Kd_plastic,Kdeg=Kdeg))
  
  #interpolate amount of AMI in medium  for the experimental timepoints
  #Control  
  Ctr_TK_int_plastic_1uM_acu<-approx(x=Ctr_TK_1_1uM[,"time"],y=Ctr_TK_1_1uM[,"Aplastic"],xout=Exp_time)
  Ctr_TK_int_medium_1uM_acu<-approx(x=Ctr_TK_1_1uM[,"time"],y=Ctr_TK_1_1uM[,"Amedium"],xout=Exp_time)
  Ctr_TK_int_plastic_1uM_7d<-approx(x=Ctr_TK_3_1uM[,"time"],y=Ctr_TK_3_1uM[,"Aplastic"],xout=168)
  Ctr_TK_int_medium_1uM_7d<-approx(x=Ctr_TK_3_1uM[,"time"],y=Ctr_TK_3_1uM[,"Amedium"],xout=168)
  Ctr_TK_int_plastic_1uM_14d<-approx(x= Ctr_TK_6_1uM[,"time"],y= Ctr_TK_6_1uM[,"Aplastic"],xout=336)
  Ctr_TK_int_medium_1uM_14d<-approx(x= Ctr_TK_6_1uM[,"time"],y= Ctr_TK_6_1uM[,"Amedium"],xout=336)
  
  
  Ctr_TK_int_plastic_2uM_acu<-approx(x=Ctr_TK_1_2uM[,"time"],y=Ctr_TK_1_2uM[,"Aplastic"],xout=Exp_time)
  Ctr_TK_int_medium_2uM_acu<-approx(x=Ctr_TK_1_2uM[,"time"],y=Ctr_TK_1_2uM[,"Amedium"],xout=Exp_time)
  Ctr_TK_int_plastic_2uM_7d<-approx(x=Ctr_TK_3_2uM[,"time"],y=Ctr_TK_3_2uM[,"Aplastic"],xout=168)
  Ctr_TK_int_medium_2uM_7d<-approx(x=Ctr_TK_3_2uM[,"time"],y=Ctr_TK_3_2uM[,"Amedium"],xout=168)
  Ctr_TK_int_plastic_2uM_14d<-approx(x= Ctr_TK_6_2uM[,"time"],y= Ctr_TK_6_2uM[,"Aplastic"],xout=336)
  Ctr_TK_int_medium_2uM_14d<-approx(x= Ctr_TK_6_2uM[,"time"],y= Ctr_TK_6_2uM[,"Amedium"],xout=336)
  
  
  Ctr_TK_int_plastic_3uM<-approx(x=Ctr_TK_optim_3uM[,"time"],y=Ctr_TK_optim_3uM[,"Aplastic"],xout=Exp_time)
  Ctr_TK_int_medium_3uM<-approx(x=Ctr_TK_optim_3uM[,"time"],y=Ctr_TK_optim_3uM[,"Amedium"],xout=Exp_time)
  
  #With cells 
  #1uM acute 48 h
  TK_int_cells_1uM<-approx(x=TK_1_1uM[,"time"],y=TK_1_1uM[,"Acells"],xout=Exp_time)
  TK_int_plastic_1uM<-approx(x=TK_1_1uM[,"time"],y=TK_1_1uM[,"Aplastic"],xout=Exp_time)
  TK_int_medium_1uM<-approx(x=TK_1_1uM[,"time"],y=TK_1_1uM[,"Amedium"],xout=Exp_time)
  #1uM 7 days
  TK_int_cells_1uM_7d<-approx(x=TK_3_1uM[,"time"],y=TK_3_1uM[,"Acells"],xout=168)
  TK_int_plastic_1uM_7d<-approx(x=TK_3_1uM[,"time"],y=TK_3_1uM[,"Aplastic"],xout=168)
  TK_int_medium_1uM_7d<-approx(x=TK_3_1uM[,"time"],y=TK_3_1uM[,"Amedium"],xout=168)
  #1uM 14 days
  TK_int_cells_1uM_14d<-approx(x=TK_6_1uM[,"time"],y=TK_6_1uM[,"Acells"],xout=336)
  TK_int_plastic_1uM_14d<-approx(x=TK_6_1uM[,"time"],y=TK_6_1uM[,"Aplastic"],xout=336)
  TK_int_medium_1uM_14d<-approx(x=TK_6_1uM[,"time"],y=TK_6_1uM[,"Amedium"],xout=336)
  
  #2uM acute 48 h
  TK_int_cells_2uM<-approx(x=TK_1_2uM[,"time"],y=TK_1_2uM[,"Acells"],xout=Exp_time)
  TK_int_plastic_2uM<-approx(x=TK_1_2uM[,"time"],y=TK_1_2uM[,"Aplastic"],xout=Exp_time)
  TK_int_medium_2uM<-approx(x=TK_1_2uM[,"time"],y=TK_1_2uM[,"Amedium"],xout=Exp_time)
  
  #Do not take in account cytotoxic concentrations
  # #2uM 7 days
  # TK_int_cells_2uM_7d<-approx(x=TK_3_2uM[,"time"],y=TK_3_2uM[,"Acells"],xout=168)
  # TK_int_plastic_2uM_7d<-approx(x=TK_3_2uM[,"time"],y=TK_3_2uM[,"Aplastic"],xout=168)
  # TK_int_medium_2uM_7d<-approx(x=TK_3_2uM[,"time"],y=TK_3_2uM[,"Amedium"],xout=168)
  # #2uM 14 days
  # TK_int_cells_2uM_14d<-approx(x=TK_6_2uM[,"time"],y=TK_6_2uM[,"Acells"],xout=336)
  # TK_int_plastic_2uM_14d<-approx(x=TK_6_2uM[,"time"],y=TK_6_2uM[,"Aplastic"],xout=336)
  # TK_int_medium_2uM_14d<-approx(x=TK_6_2uM[,"time"],y=TK_6_2uM[,"Amedium"],xout=336)
  # 
  # #3uM acute 48 h
  # TK_int_cells_3uM<-approx(x=TK_optim_3uM[,"time"],y=TK_optim_3uM[,"Acells"],xout=Exp_time)
  # TK_int_plastic_3uM<-approx(x=TK_optim_3uM[,"time"],y=TK_optim_3uM[,"Aplastic"],xout=Exp_time)
  # TK_int_medium_3uM<-approx(x=TK_optim_3uM[,"time"],y=TK_optim_3uM[,"Amedium"],xout=Exp_time)
  # 
  
  #Describe RSS (residual sum of squares) function
  #First calculate separatly the RSS for each comparment
  
  RSS_Ctr_plastic=1*mean(c(
    abs(((Exp_1uMC_plastic - Ctr_TK_int_plastic_1uM_acu$y)/Exp_1uMC_plastic)),
    abs(((Exp_2uMC_plastic - Ctr_TK_int_plastic_2uM_acu$y)/Exp_2uMC_plastic)),
    abs(((Exp_3uMC_plastic - Ctr_TK_int_plastic_3uM$y)/Exp_3uMC_plastic)),
    abs(((Eval_1uMC_plastic[1] - Ctr_TK_int_plastic_1uM_7d$y)/Eval_1uMC_plastic[1])),
    abs(((Eval_1uMC_plastic[2] - Ctr_TK_int_plastic_1uM_14d$y)/Eval_1uMC_plastic[2])),
    abs(((Eval_2uMC_plastic[1] - Ctr_TK_int_plastic_2uM_7d$y)/Eval_2uMC_plastic[1])),
    abs(((Eval_2uMC_plastic[2] - Ctr_TK_int_plastic_2uM_14d$y)/Eval_2uMC_plastic[2]))
  ))
  
  RSS_Ctr_medium=1*mean(c(
    abs(((Exp_1uMC_medium - Ctr_TK_int_medium_1uM_acu$y)/Exp_1uMC_medium)),
    abs(((Exp_2uMC_medium - Ctr_TK_int_medium_2uM_acu$y)/Exp_2uMC_medium)),
    abs(((Exp_3uMC_medium - Ctr_TK_int_medium_3uM$y)/Exp_3uMC_medium)),
    abs(((Eval_1uMC_medium[1] - Ctr_TK_int_medium_1uM_7d$y)/Eval_1uMC_medium[1])),
    abs(((Eval_2uMC_medium[1] - Ctr_TK_int_medium_2uM_7d$y)/Eval_2uMC_medium[1]))
  ))
  
  RSS_cells=1*mean(c(
    abs(((Exp_1uM_cells - TK_int_cells_1uM$y)/Exp_1uM_cells)),
    abs(((Exp_2uM_cells - TK_int_cells_2uM$y)/Exp_2uM_cells )),
    #abs(((Exp_3uM_cells - TK_int_cells_3uM$y)/Exp_2uM_cells )),
    abs(((Exp_1uM_cells[1] - TK_int_cells_1uM_7d$y)/Exp_1uM_cells[1])),
    #abs(((Exp_2uM_cells[1] - TK_int_cells_2uM_7d$y)/Exp_2uM_cells[1])),
    abs(((Exp_1uM_cells[2] - TK_int_cells_1uM_14d$y)/Exp_1uM_cells[2]))
    # abs(((Exp_2uM_cells[2] - TK_int_cells_2uM_14d$y)/Exp_2uM_cells[2]))
  ))
  
  RSS_medium=1*mean(c(
    abs(((Exp_1uM_medium - TK_int_medium_1uM$y)/Exp_1uM_medium)),
    abs(((Exp_2uM_medium - TK_int_medium_2uM$y)/Exp_2uM_medium)),
    # abs(((Exp_3uM_medium - TK_int_medium_3uM$y)/Exp_3uM_medium)),
    abs(((Eval_1uM_medium[1] - TK_int_medium_1uM_7d$y)/Eval_1uM_medium[1]))
    # abs(((Eval_2uM_medium[1] - TK_int_medium_2uM_7d$y)/Eval_2uM_medium[1]))
  ))
  
  RSS_plastic=1*mean(c(
    abs(((Exp_1uM_plastic - TK_int_plastic_1uM$y)/Exp_1uM_plastic)),
    abs(((Exp_2uM_plastic - TK_int_plastic_2uM$y)/Exp_2uM_plastic)),
    #abs(((Exp_3uM_plastic - TK_int_plastic_3uM$y)/Exp_3uM_plastic)),
    abs(((Eval_1uM_plastic[1] - TK_int_plastic_1uM_7d$y)/Eval_1uM_plastic[1])),
    abs(((Eval_1uM_plastic[2] - TK_int_plastic_1uM_14d$y)/Eval_1uM_plastic[2]))
    # abs(((Eval_2uM_plastic[1] - TK_int_plastic_2uM_7d$y)/Eval_2uM_plastic[1])),
    # abs(((Eval_2uM_plastic[2] - TK_int_plastic_2uM_14d$y)/Eval_2uM_plastic[2]))
  ))
  
  #Sum all RSS
  RSS_all=RSS_Ctr_plastic+RSS_Ctr_medium+RSS_cells*2+RSS_medium+RSS_plastic
  print(c("Ka_plastic"=Ka_plastic,"Kd_plastic"=Kd_plastic, "Kdeg"=Kdeg,"Ka_cells"=Ka_cells,"Kd_cells"=Kd_cells,"RSS_all"=RSS_all))
  return(RSS_all)
}

#The error function is minmized with the package optim 
#Optim basically teste different values for the parameters and calculates the error(RSS
#Then it starts chosing parameters such that the error keeps decreasing (being minimal) till is not longer possible
library(deSolve)

#initial value sof parameters to be optimized
initial_Ka_plastic=  0.3193
initial_Kd_plastic=  0.1364
initial_Kdeg=  0.01
initial_Ka_cells= 0.2834
initial_Kd_cells=  0.2741

#optimization
  output<-optim(c(initial_Ka_plastic,initial_Kd_plastic,initial_Kdeg,initial_Ka_cells,initial_Kd_cells),
                vallerror,#control = list(maxit=50),
                method="L-BFGS-B",lower=c(1E-6,1E-6,1e-6,1E-6,1E-6),upper=c(3,3,1,2,3))
  
