#This code described the functions for TK model that will simulate the control and the cells condition
#It also contains the input parameters and experimental data
#Author:Susana Proenca, June 2023

###
#Experimental sets -------------------------------------------------------------
###

#Amounts of amiodarone are presented in nmoles (originally in umol but converted by multiplying 1000)

#WITHOUT CELLS#

#Acute experimental data
Exp_1uMC_medium<-1000*c(0.000853333,0.000724,0.000672,0.000658667,0.000385333)
STD_Exp_1uMC_medium<-1000*c(8.39365E-05,9.80612E-05,6.97424E-05,5.43078E-05,2.94845E-05)
Exp_1uMC_plastic<-1000*c(0.000586,0.000654667,0.000744,0.000852667,0.000859333)
STD_Exp_1uMC_plastic<-1000*c(1.249E-05,4.22532E-05,8.48528E-06,2.89367E-05,1.22202E-05)

Exp_2uMC_medium<-1000*c(0.002432,0.001958667,0.002126667,0.001804,0.001268)
STD_Exp_2uMC_medium<-1000*c(0.000372838,5.82866E-05,0.000252042,0.000322019,0.000052)
Exp_2uMC_plastic<-1000*c(0.000900667,0.001024,0.001136,0.001435333,0.001398667)
STD_Exp_2uMC_plastic<-1000*c(8.10021E-05,0.000165662,1.0583E-05,1.61658E-05,5.70029E-05)

Exp_3uMC_medium<-1000*c(0.004153333,0.003716,0.004097333,0.003450667,0.002041333)
STD_Exp_3uMC_medium<-1000*c(0.000334194,0.000295594	,	0.000294329,0.000102163)
Exp_3uMC_plastic<-1000*c(0.001187333,0.001438667,0.001636667,0.001937333,0.001906)
STD_Exp_3uMC_plastic<-1000*c(5.55098E-05,5.71431E-05,3.97157E-05,7.74683E-05,6.3498E-05)

#Repeated experimental data
Eval_1uMC_medium=1000*c(0.000766667,0)
STD_Eval_1uMC_medium=1000*c(6.88573E-05,0)
Eval_1uMC_plastic=1000*c(0.001092,0.000625333)
STD_Eval_1uMC_plastic=1000*c(4.3589E-05,2.41109E-05)

Eval_2uMC_medium=1000*c(0.001844,0)
STD_Eval_2uMC_medium=1000*c(7.78717E-05,0)
Eval_2uMC_plastic=1000*c(0.001811333,0.000928667)
STD_Eval_2uMC_plastic=1000*c(9.00074E-05,8.28573E-05)

#WITH CELLS#

#Acute experimental data for plates with cells
Exp_1uM_medium<-1000*c(0.000685333,0.000324,0.000249333,0.000166667,0.000234667)
STD_Exp_1uM_medium<-1000*c(3.69504E-05,2.1166E-05,4.63609E-05,4.49592E-05,1.61658E-05)
Exp_1uM_cells<-1000*c(0.000181083,0.000353667,0.000618625,0.000678625,0.000386)
STD_Exp_1uM_cells<-1000*c(4.62644E-05,6.38065E-05,0.000137002,8.22012E-05,9.82878E-05)
Exp_1uM_plastic<-1000*c(0.000538,0.000533333,0.000583,0.000533333,0.000600667)
STD_Exp_1uM_plastic<-1000*c(1.9799E-05,1.10151E-05,1.27279E-05,3.91067E-05,3.52326E-05)

Exp_2uM_medium<-1000*c(0.001766667,0.001161333,0.000884,0.000841333,0.000606667)
STD_Exp_2uM_medium<-1000*c(0.000288675,3.05505E-05,0.000135115,0.000257257,9.87387E-05)
Exp_2uM_cells<-1000*c(0.000471583,0.000911583,0.00115825,0.001289417,0.0013425)
STD_Exp_2uM_cells<-1000*c(3.6333E-05,4.89524E-05,5.4044E-05,0.000173221,1.37886E-05)
Exp_2uM_plastic<-1000*c(0.000808,0.000844667,0.000954667,0.000958667,0.000998667)
STD_Exp_2uM_plastic<-1000*c(0.000127953,0.000111648,6.71218E-05,0.000107151,2.01329E-05)

Exp_3uM_medium<-1000*c(0.002693333,0.001992,0.001393333,0.001616,0.001272)
STD_Exp_3uM_medium<-1000*c(0.000252486,8.66256E-05,0.000178945,0.000290517,0.000215444)
Exp_3uM_cells<-1000*c(0.000862167,0.001398917,0.001929,0.001664667,0.001368583)
STD_Exp_3uM_cells<-1000*c(6.4008E-05,0.000142816,3.37898E-05,9.0393E-05,0.000248968)
Exp_3uM_plastic<-1000*c(0.00116,0.001156667,0.001229333,0.001539333,0.001504)
STD_Exp_3uM_plastic<-1000*c(0.0001202,4.99733E-05,3.06159E-05,0.000112718,9.67678E-05)

#Experimental data repeated exposure in plates with cells
#experimental repeated 1 uM 
Eval_1uM_medium=1000*c(0.00040266,0)
STD_Eval_1uM_medium=1000*c(0.000402667,0)
Eval_1uM_cells=1000*c(0.001128083,0.0005405)
STD_Eval_1uM_cells=1000*c(0.000142401,9.56971E-05)
Eval_1uM_plastic=1000*c(0.000989333,0.000594)
STD_Eval_1uM_plastic=1000*c(0.000145222,2E-06)

#experimental repeated 2 uM 
Eval_2uM_medium=1000*c(0.001052,0)
STD_Eval_2uM_medium=1000*c(0.000186419,0)
Eval_2uM_cells=1000*c(0.002146417,0.000640167)
STD_Eval_2uM_cells=1000*c(3.99291E-05,0.000266535)
Eval_2uM_plastic=1000*c(0.001429333,0.000971333)
STD_Eval_2uM_plastic=1000*c(0.000118361,9.20072E-05)

#Amount dosed in medium as measured analytically--------------------------------
Dose_1uM<-0.001516*1000   # in nmoles
Dose_2uM<-0.002952*1000
Dose_3uM<-0.005144*1000

#Experimental timepoints--------------------------------------------------------
Exp_time<-c(1,3,6,24,48)
Eval_time<-c(168,336)

###
#Models-------------------------------------------------------------------------
###

#Compartmental model for control plate
TK_model_1<-function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    
    dAmedium=(-Ka_plastic*Amedium)+(Kd_plastic*Aplastic)-Kdeg*Amedium
    dAplastic=(Ka_plastic*Amedium)+(-Kd_plastic*Aplastic)
    dAdeg=Kdeg*Amedium
    
    list(c(dAmedium,dAplastic,dAdeg))
  })
}

#Compartmental model for plate with cells
TK_model_2<-function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    
    dAmedium=(-Ka_plastic*Amedium)+(Kd_plastic*Aplastic)-Kdeg*Amedium+(Kd_cells*Acells-Ka_cells*Amedium)
    dAplastic=(Ka_plastic*Amedium)+(-Kd_plastic*Aplastic)
    dAcells=(Ka_cells*Amedium-Kd_cells*Acells)
    list(c(dAmedium,dAplastic,dAcells))
  })
}
