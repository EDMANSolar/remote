# load meteo data
load('meteoReduced.RData')

#################################################################
## Local Models
#################################################################

# Liu and Scott 2001
LiuScott2001_local <- lapply(c(1:4), function(j){ 
  BTd <- as.Date(meteo[[j]]$Date, '%d/%m/%y')
  train <- subset(meteo[[j]], meteo[[j]]$Year!=2013)
  test <- subset(meteo[[j]], meteo[[j]]$Year==2013)
  
  LS1 <- function(x){a <- x[1]; b <-  x[2];  d <-  x[3];  e <-  x[4];  f <-  x[5];  g <-  x[6]
                     
                     Suma <- sum((train$Rad - g - train$Bo0d*a*(1 - exp( -b*(train$DeltaT^d)))*
                                    (1 + e*train$M_menos1 + f*train$M + g*train$M_mas1))^2)
                     return(Suma)
  }
  S <- optimx(c(0.1, 0.2,0.2,0.1,0.1,0.1,0.1), LS1, method=c("ucminf"))
  
  a<-as.numeric(S[1]); b<-as.numeric(S[2]); d<-as.numeric(S[3]); e<-as.numeric(S[4]); f<-as.numeric(S[5]);
  g<-as.numeric(S[6])
  
  Rad_test <- a*(1-exp(-b*(test$DeltaT^d)))*test$Bo0d*(1+e*test$M_menos1+f*test$M+g*test$M_mas1)+g
  Rad_test
})

# Antonanzas-Torres et al. (2013)
AntonanzasTorres.SanzGarciaea2013_local <- lapply(c(1:4), function(j){ 
  BTd <- as.Date(meteo[[j]]$Date,'%d/%m/%y')
  train <- subset(meteo[[j]],meteo[[j]]$Year!=2013)
  test <- subset(meteo[[j]],meteo[[j]]$Year==2013)
  
  AN <- function(x){a <- x[1]; b <- x[2]; d <- x[3]; e <- x[4]; f <- x[5]; g <- x[6]; h <- x[7]; i <- x[8]; k <- x[9]
                    Suma <- sum((train$Rad-k-train$Bo0d*a*(1-exp(-b*(train$DeltaT^d)))*
                                   (1+e*train$M_menos1+f*train$M+g*train$M_mas1+h*train$DeltaT_mas1+
                                      i*train$DeltaT_menos1))^2)
                    return(Suma)
  }
  S <- optimx(c(0.1,0.2,0.2,0.1,0.1,0.1,0.1,0.1,0.1), AN, method="ucminf")
  
  a<-as.numeric(S[1]); b<-as.numeric(S[2]); d<-as.numeric(S[3]); e<-as.numeric(S[4]); f<-as.numeric(S[5]);
  g<-as.numeric(S[6]);h<-as.numeric(S[7]);i<-as.numeric(S[8]);k<-as.numeric(S[9])
 
  # testing error
  Rad_test <- test$Bo0d*a*(1-exp(-b*(test$DeltaT^d)))*(1+e*test$M_menos1+f*test$M+g*test$M_mas1+
                                                         h*test$DeltaT_mas1+i*test$DeltaT_menos1)+k
  Rad_test
})

#################################################################
## General Model
#################################################################

meteo_collated <- do.call(rbind, meteo)
BTd <- as.Date(meteo_collated$Date, '%d/%m/%y')
# database for general model training
train <- subset(meteo_collated, meteo_collated$Year!=2013)
# testing performed in each of the four stations
test_st <- lapply(meteo, function(x){test <- subset(x, x$Year==2013)})

# Liu and Scott 2001 function
LS1 <- function(x){a <- x[1]; b <-  x[2];  d <-  x[3];  e <-  x[4];  f <-  x[5];  g <-  x[6]
                   
                   Suma <- sum((train$Rad - g - train$Bo0d*a*(1 - exp(-b*(train$DeltaT^d)))*
                                  (1 + e*train$M_menos1 + f*train$M + g*train$M_mas1))^2)
                   return(Suma)
}

# parameter definition
S <- optimx(c(0.1, 0.2, 0.2, 0.1, 0.1, 0.1, 0.1), LS1, method=c("ucminf"))
a <- as.numeric(S[1]); b <- as.numeric(S[2]); d <- as.numeric(S[3]); e <- as.numeric(S[4]); f <- as.numeric(S[5]);
g <- as.numeric(S[6])

# evaluation for each station in the testing period
LiuScott2001_general <- sapply(c(1:4), function(x){
  Rad_test <- a*(1 - exp(-b*(test_st[[x]]$DeltaT^d)))*
    test_st[[x]]$Bo0d*(1 + e*test_st[[x]]$M_menos1 + f*test_st[[x]]$M + g*test_st[[x]]$M_mas1) + g
  Rad_test
})


# Antonanzas-Torres et al. (2013)
AN <- function(x){a <- x[1]; b <- x[2]; d <- x[3]; e <- x[4]; f <- x[5]; g <- x[6]; h <- x[7]; i <- x[8]; k <- x[9]
                  Suma <- sum((train$Rad-k-train$Bo0d*a*(1-exp(-b*(train$DeltaT^d)))*
                                 (1+e*train$M_menos1+f*train$M+g*train$M_mas1+h*train$DeltaT_mas1+
                                    i*train$DeltaT_menos1))^2)
                  return(Suma)
}

# parameter definition
S <- optimx(c(0.1,0.2,0.2,0.1,0.1,0.1,0.1,0.1,0.1), AN, method="ucminf")
a<-as.numeric(S[1]); b<-as.numeric(S[2]); d<-as.numeric(S[3]); e<-as.numeric(S[4]); f<-as.numeric(S[5]);
g<-as.numeric(S[6]);h<-as.numeric(S[7]);i<-as.numeric(S[8]);k<-as.numeric(S[9])

AntonanzasTorres.SanzGarciaea2013_general <- sapply(c(1:4),function(x){
  Rad_test <- test_st[[x]]$Bo0d*a*(1 - exp(-b*(test_st[[x]]$DeltaT^d)))*
    (1 + e*test_st[[x]]$M_menos1 + f*test_st[[x]]$M + g*test_st[[x]]$M_mas1 +
     h*test_st[[x]]$DeltaT_mas1 + i*test_st[[x]]$DeltaT_menos1) + k
  Rad_test
})