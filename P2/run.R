source("sa_daily_all.R")
source("sa_daily_one.R")
source("sa_hourly_all.R")
source("sa_hourly_one.R")
source("pred_daily_one.R")
source("pred_daily_all.R")
source("pred_hourly_one.R")
source("pred_hourly_all.R")
source("mc_daily_all.R")
source("mc_daily_one.R")
source("mc_hourly_all.R")
source("mc_hourly_one.R")
source("graph_all.R")
source("graph_one.R")

run <- function(){
  v <- readline("Qual o tipo de optimizacao temporal que pertende fazer? \n[1] Diaria, [2] Horaria \n")
  x <- readline("Qual o tipo de optimizacao genero que pertende fazer? \n[1] Male, [2] Female, [3] Unisex, [4] All \n")  
  y <- readline("Qual o periodo que pertende optimizar? \n[1] 2/12 a 8/12, [2] 9/12 a 15/12, [3] 16/12 a 22/12 \n")
  z <- readline("Qual o limite de custos da campanha? \n")
  w <- readline("Qual o tipo de optimizacao? \n [1] Simulated Annealing, [2] Monte Carlo, [3] Todas\n")

  x <- as.numeric(unlist(strsplit(x, ",")))
  y <- as.numeric(unlist(strsplit(y, ",")))
  z <- as.numeric(unlist(strsplit(z, ",")))
  w <- as.numeric(unlist(strsplit(w, ",")))
  v <- as.numeric(unlist(strsplit(v, ",")))
  
  n=0
  if(v==1){
	if(y==1){ n=14 }
	if(y==2){ n=7 }
	if(x==1|x==2|x==3){
	pred_daily_vec(n,x)
	if(w==1){ sa_daily_vec(x,z,w)}
	if(w==2){ mc_daily_vec(x,z,w)}
	if(w==3){ sa_daily_vec(x,z,w); mc_daily_vec(x,z,w); graph_one(x)} 
	}
	if(x==4){
	pred_daily_mat(n)
	if(w==1){ sa_daily_mat(w,x,z) }
	if(w==2){ mc_daily_mat(w,x,z) }
	if(w==3){ sa_daily_mat(w,x,z); mc_daily_mat(w,x,z); graph_all() }
	}
  }
  if(v==2){
	if(y==1){ n=168 }
	if(y==2){ n=84 }
	if(x==1|x==2|x==3){
	pred_hourly_vec(n,x)
	if(w==1){ sa_hourly_vec(x,z)}
	if(w==2){ mc_hourly_vec(x,z)}
	if(w==3){ sa_hourly_vec(x,z); mc_hourly_vec(x,z)} 
	}
	if(x==4){
	pred_hourly_mat(n)
	if(w==1){ sa_hourly_mat(z) }
	if(w==2){ mc_hourly_mat(z) }
	if(w==3){ sa_hourly_mat(z); mc_hourly_mat(z) }
	}
  }

}





