#This code is to perform optimization of amiodarone TK model
#Now the error function is used with package optim to fit the plastic and cells sorption parameters
#Optim basicly teste different values for the parameters and calculates the error(RSS
#Then it starts chosing parameters such that the error keeps decreasing (being minimal) till is not longer possible
#The optimization is herein scripted to be done "N_test" times
#this should result in  roughly the same optimized values or else it means the optimization algorithm is not converging and/or we are stuck with local minimums
library(deSolve)
N_tests = 1

#Define range of started values for the parameters
Ka_plastic_min = 0.000005
Ka_plastic_max = 1

Kd_plastic_min = 0.000005
Kd_plastic_max = 1

Kdeg_min=1E-8
Kdeg_max=0.5

Ka_cells_min=1E-8
Ka_cells_max=1

Kd_cells_min=1E-8
Kd_cells_max=1

#Generate random initial numbers for each parameter

# initial_Ka_plastic=  runif(N_tests,min=Ka_plastic_min,max=Ka_plastic_max)
# initial_Kd_plastic=  runif(N_tests,min=Kd_plastic_min,max=Kd_plastic_max)
# initial_Nmax= runif(N_tests, min=Nmax_min,max=Nmax_max)
# initial_Kdeg= runif(N_tests, min=Kdeg_min,max=Kdeg_max)
# initial_Ka_cells=  runif(N_tests,min=Ka_cells_min,max=Ka_cells_max)
# initial_Kd_cells=  runif(N_tests,min=Kd_cells_min,max=Kd_cells_max)
initial_Ka_plastic=  0.3193
initial_Kd_plastic=  0.1364
initial_Nmax= 2.23
initial_Kdeg=  0.01
initial_Ka_cells= 0.2834
initial_Kd_cells=  0.2741

#Create vectors to save values
rmse = seq(0,0,length.out=N_tests)
test_Ka_plastic = seq(0,0,length.out=N_tests)
test_Kd_plastic = seq(0,0,length.out=N_tests)
test_Kdeg = seq(0,0,length.out=N_tests)
test_Ka_cells = seq(0,0,length.out=N_tests)
test_Kd_cells = seq(0,0,length.out=N_tests)
test_conv= seq(0,0,length.out=N_tests)
test_message = seq(0,0,length.out=N_tests)

#Create dataframes to store values
result_optim=data.frame(rmse,test_Ka_plastic,test_Kd_plastic,test_Kdeg,test_Ka_cells,test_Kd_cells,test_conv,test_message)

#Make optimization and save values for N_tests
for(i in 1:N_tests){
  print(cat("Current iteration: ", i))
  output<-optim(c(initial_Ka_plastic[i],initial_Kd_plastic[i],initial_Kdeg[i],initial_Ka_cells[i],initial_Kd_cells[i]),
                vallerror,#control = list(maxit=50),
                method="L-BFGS-B",lower=c(1E-6,1E-6,1e-6,1E-6,1E-6),upper=c(3,3,1,2,3))
  
  result_optim[i,1] = output[[2]]
  result_optim[i,2] = output[[1]][1]
  result_optim[i,3] = output[[1]][2]
  result_optim[i,4] = output[[1]][3]
  result_optim[i,5] = output[[1]][4]
  result_optim[i,6] = output[[1]][5]
  result_optim[i,7] = output[[1]][6]
  result_optim[i,8] = output[[4]]
  result_optim[i,8] = output[[5]]
}