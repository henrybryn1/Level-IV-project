grp_gendat_bin <- function(seed, n, cor, ngroup, nper){
  
  if (match("MASS",.packages(),0)==0) require(MASS)
  set.seed(seed)
  #outcome
  beta <- c(-1.8, -1, -0.25, 0.25, 1,-0.5, -0.1, 0.1, 0.5, 0.2, 0.2, 0.2, 0.2, 0, 0, 0, 0, 0, 0, 0, 0)
  #selection
  gamma <- c(1.24, -0.6, -0.3, 0.3, 0.6, -0.5, -0.2, 0.2, 0.5, 0, 0, 0, 0, 0.15, 0.15, 0.15, 0.15, 0, 0, 0, 0)
  
  rho <- 0.5
  p <- length(beta)-1
  
  if(ngroup*nper != p){stop("!!!error ngroup*nper != p")}
  
  Sig <- matrix(0, ncol=ngroup, nrow=ngroup)
  for(index in 1:ngroup){
    Sig[index,] <-  c(rev(cumprod(rep(rho,index-1))),1,cumprod(rep(rho,ngroup-index)))
  }
  
  Z <- matrix(rnorm(ngroup*n), ncol=ngroup)
  Z <- crossprod(t(Z), chol(Sig))
  
  R <- matrix(rnorm(ngroup*nper*n), ncol = p)

  
  bigZ <- matrix(rep(as.numeric(t(Z)), each = nper), nrow = nrow(Z), byrow = TRUE)
  
  
  X <- (R+bigZ)/sqrt(2)
  
  meane <- c(0,0)
  
  cove <- matrix(c(1,cor,cor,1),nrow=2)
  e <- mvrnorm(n,meane,cove)
  
  X <- cbind(1, X)
  ystar <- ((X%*%beta)+e[,1])
  
  yy <-  ifelse(ystar > 0, 1,0)
  #gw <- cbind(1,X)
  
  ustar <- ((X%*%gamma)+e[,2])
  uu <- ifelse(ustar > 0, 1, 0)
  
  yobs <- yy*uu
  covar <- cov(X)
  
  #yobs <- factor(yobs)
  dat<-data.frame(yy,yobs,uu,ystar,X[,-1])
  
  return(dat)
  
}

##########################################################################
############                 TOOLS                   #####################
##########################################################################

makePD = function(mat){
  N = nrow(mat)
  HC = mat
  D = eigen(mat)
  E = D$values
  U = D$vectors
  v = as.numeric(E < 0)
  idx = which(v==1)
  m = sum(v) # number of negative values
  if(m > 0){
    S = sum(v*E)*2
    W = (S*S*100)+1
    P = min(abs(E[idx])) # smallest positive value
    for(i in idx){
      C = E[i]
      E[i] = P * (S-C)*(S-C)/W
    }
  }
  return(E)
}


pseudo_data <- function(YS, XS, YO, XO){
  if (match("GJRM",.packages(),0)==0) require(GJRM)
  nbeta <- dim(XS)[2] + dim(XO)[2] + 1
  #b0 <- rep(0.5, nbeta)
  #backup
  #b <- optim(b0 ,fn=log_lik, YS=YS,XS=XS,YO=YO,XO=XO, hessian=T,method="BFGS")
  #covb <- solve(b$hessian)
  #coefs <- b$par
  
  predata <- list(YS, YO, XS, XO)
  
  fit <-  gjrm(formula = list(YS ~ XS[,-1], YO ~ XO[,-1]), data = predata, model = "BSS", copula = "N", margins = c("probit","probit"))
  coefs <- fit$coefficients
  covb <- solve(fit$He)
  
  #do not fully understand. Cholesky?
  e <- eigen(covb)
  aa <- e$vectors %*% diag(1/sqrt(abs(e$values))) %*% t(e$vectors)
  bb <- e$vectors %*% diag(1/sqrt(makePD(covb))) %*% t(e$vectors)
  sigma2 <-  ifelse((det(covb) > 0),list(aa),list(bb))
  xstar <- sigma2[[1]]
  colnames(xstar) <- colnames(covb)
  ystar <- xstar%*%as.vector(coefs)
  XY <- list(xstar,ystar)
  names(XY) <- c("xstar","ystar")
  return(XY)
}


#This function produces orthonormal X for the group descent algorithm
normalise <- function(X){
  n <- nrow(X)
  p <- ncol(X)
  
  decomp <- svd((1/n)*t(X)%*%X)
  #decomp$u%*%diag(decomp$d)%*%t(decomp$v)
  
  Xtild <- X%*%decomp$u%*%diag(1/sqrt(decomp$d))
  
  return(list(Xtild = Xtild, Q = decomp$u, invrtlam = diag(1/sqrt(decomp$d))))
}



#this function is to return the "actual" beta for unnormalised X
beta_denorm <- function(beta_tild, Q, invrtlam){
  beta <- Q%*%invrtlam%*%beta_tild
  return(beta)
}

#soft-thresholding operator
S <- function(z, lambda) {
  val <- ifelse(z > lambda, z - lambda, ifelse(z < -lambda, z + lambda, 0))
  return(val)
}

#Group lasso oprator
grp_S_thresh <- function(z, lambda, gamma){
  znorm <- as.numeric(sqrt(t(z)%*%z))
  val <- S(znorm, lambda)*z/znorm
  return(val)
}


#MCP firm thresholding
Fthresh <- function(z, lambda, gamma){
  val <- ifelse(abs(z)<gamma*lambda, (S(z, lambda)/(1-1/gamma)), z)
  return(val)
}

#SCAD firm thresholding
Fs <- function(z, lambda, gamma){
  val <- ifelse(abs(z)<2*lambda, S(z, lambda), ifelse(abs(z)<=gamma*lambda, (S(z, (gamma*lambda)/(gamma - 1))/(1-1/(gamma-1))), z))
}

#group mcp
grp_F <- function(z, lambda, gamma){
  znorm <- as.numeric(sqrt(t(z)%*%z))
  val <- Fthresh(znorm, lambda, gamma)*z/znorm
  return(val)
}

#group SCAD
grp_Fs <- function(z, lambda, gamma){
  znorm <- as.numeric(sqrt(t(z)%*%z))
  val <- Fs(znorm, lambda, gamma)*z/znorm
  return(val)
}


#########################################################################
#########################################################################
#########################################################################


log_lik <- function(beta,YS,XS,YO,XO){
  if (match("pbivnorm",.packages(),0)==0) require(pbivnorm)
  nObs <- length(YS)
  
  i00 <- YS == 0
  i10 <- (YS == 1) & (YO == 0)
  i11 <- (YS == 1) & (YO == 1)
  NXS <- ncol(XS)
  NXO <- ncol(XO)
  ibetaS <- 1:NXS
  ibetaO <- seq(tail(ibetaS, 1) + 1, length = NXO)
  irho <- tail(ibetaO, 1) + 1
  betaS <- beta[ibetaS]
  betaO <- beta[ibetaO]
  
  ############no wrapper
  rho <- tanh(beta[irho])
  
  XS00 <- drop(XS[i00, , drop = FALSE] %*% betaS)
  XS10 <- drop(XS[i10, , drop = FALSE] %*% betaS)
  XS11 <- drop(XS[i11, , drop = FALSE] %*% betaS)
  XO10 <- drop(XO[i10, , drop = FALSE] %*% betaO)
  XO11 <- drop(XO[i11, , drop = FALSE] %*% betaO)
  loglik <- numeric(nObs)
  loglik[i00] <-  pnorm(-XS00, log.p = TRUE)
  rho_rep <- rep(rho, length(XS10))
  f2 <- pbivnorm(XS10, -XO10, -rho_rep)
  loglik[i10] <- log(f2)
  rho_rep2 <- rep(rho, length(XS11))
  f3 <- pbivnorm(XS11, XO11,  rho_rep2)
  loglik[i11] <-  log(f3)
  return(-sum(loglik))
}


#coordinate descent algorithm
descent_alg <- function(ystar, orthX, beta, Xjs, betajs, lambda, gamma, grp_thresh){
  r <- ystar - orthX%*%beta
  zjs <- list()
  stopcheck <- FALSE
  K <- sapply(Xjs, ncol)
  for(i in 1:length(K)){if(is.null(K[[i]])){K[[i]] <- 1}}
  newbetajs <- list()
  iter <- 0
  while(stopcheck == FALSE){
  #for(a in 1:100)  
    for(i in 1:length(Xjs)){
      if(i == 1){
        zj <- t(Xjs[[i]])%*%r/dim(orthX)[1] + betajs[[i]]
        newbeta <- zj
        newr <- r - Xjs[[i]]%*%(newbeta-betajs[[i]])
        newbetajs[[i]] <- newbeta
        r <- newr
      }else{
        zj <- t(Xjs[[i]])%*%r/dim(orthX)[1] + betajs[[i]]
        newbeta <- grp_thresh(zj,lambda*sqrt(K[[i]]), gamma)
        newr <- r - Xjs[[i]]%*%(newbeta-betajs[[i]])
        newbetajs[[i]] <- newbeta
        r <- newr
      }
    }
    #check convergence of each group of coefficients
    convcheck <- rep(FALSE,length(betajs))
    for(j in 1:length(betajs)){
      if(all(abs(newbetajs[[j]]-betajs[[j]])<rep(0.0000001, length(betajs[j])))){convcheck[j] <- TRUE}else{convcheck[j] <- FALSE}
    }
    betajs <- newbetajs
    
    if(all(convcheck)){stopcheck <- TRUE}else{stopcheck <- FALSE}
    iter <- iter+1
  }
  fit <- list(betajs, iter)
  
  return(fit)
}




#This function produces produces the correctly orthonormalised groups for x and beta
data_prep <- function(YS, XS, YO, XO, group){
#  print(group)
  s <- dim(XS)[2]
  o <- dim(XO)[2]
  
  pdat <- pseudo_data(YS, XS, YO, XO)
  ystar <- pdat$ystar
  xstar <- pdat$xstar
  
  #tracks the group of each variable in the pseudo-data
  grptrack <- numeric(dim(xstar)[2])
  colnames(xstar) <- c(colnames(XS),colnames(XO),"rho")

  for(i in 1:dim(xstar)[2]){
    if(colnames(xstar)[i] %in% c("(Intercept)","rho")){
      grptrack[i] <- 0
    }else{
      if(i<=dim(XS)[2]){
        grptrack[i] <- group[colnames(xstar)[i]]
      }else if(i>dim(XS)[2]){
        #print(group[colnames(xstar)[i]] + length(unique(group)))
        grptrack[i] <- group[colnames(xstar)[i]] + length(unique(group))
        
      
      }
    }
    
  }
  
  #using ridge regression will allow to work for high dim data
  #but glmnet with lambda = 0 gives different results? maybe just use values from pdat (converted)
  
  #initmod <- glmnet(x = pdat$xstar, y = pdat$ystar, alpha = 0, lambda = 0.000000)
  #creates and labels orthonormal sub matrices of groups
  Xjs <- list()
  betajs <- list()
  jnorm <- list()
  
  
  for(i in 1:(length(unique(grptrack)))){
    if(length(which(grptrack == unique(grptrack)[i])) >1){
      normed <- normalise(xstar[,which(grptrack == unique(grptrack)[i])])
      newsub <- normed$Xtild
      jnorm[[i]] <- normed[2:3]
      assign(paste("X", as.character(unique(grptrack)[i]), sep = ""), get("newsub"))
      Xjs[[i]] <- get(paste("X", as.character(unique(grptrack)[i]), sep = ""))
      names(Xjs)[i] <- paste("X", as.character(unique(grptrack)[i]), sep = "")
    }else{
      Xjs[[i]] <- xstar[,which(grptrack == unique(grptrack)[i])]
      jnorm[[i]] <- as.numeric(sqrt(length(ystar))/sqrt(t(Xjs[[i]])%*%Xjs[[i]]))
      Xjs[[i]] <- Xjs[[i]]*jnorm[[i]]
      names(Xjs)[i] <- paste("X", as.character(unique(grptrack)[i]), sep = "")
      }
  }
  orthX <- matrix(0, nrow = dim(xstar)[1], ncol = dim(xstar)[2])
  for(i in 1:length(unique(grptrack))){
    orthX[,which(grptrack == unique(grptrack)[i])] <- Xjs[[i]] 
  }
  
  
  #initial beta values
  initmod <- lm(ystar~orthX-1, data = pdat)
  beta <- initmod$coefficients
  
  for(i in 1:length(unique(grptrack))){
    subbeta <- beta[which(grptrack == unique(grptrack)[i])]
    assign(paste("Beta", as.character(unique(grptrack)[i]), sep = ""), get("subbeta"))
    betajs[[i]] <- get(paste("Beta", as.character(unique(grptrack)[i]), sep = ""))
    names(betajs)[i] <- paste("Beta", as.character(unique(grptrack)[i]), sep = "")
  }
  
  prepdat <- list(ystar, Xjs, beta, betajs, grptrack, jnorm, orthX, xstar)
  return(prepdat)
}


biccheck <- function(beta, YS, XS, YO, XO){
  n <- length(YS)
  #degrees of freedom different in this model?
  df  <- sum(beta !=0 )-1
  lik <- log_lik(beta,YS,XS,YO,XO)
  bic <- 2*lik + log(n)*df
  return(bic)
}


lambda_select <- function(YS, XS, YO, XO, group, penalty=c("grLasso", "grMCP", "grSCAD"), gamma = 0, lambda = NULL, lambda.min = 0.01, n.lambda = 100, lambda.max = 1, full = FALSE){
  datalist <- data_prep(YS, XS, YO, XO, group)
  ystar <- datalist[[1]]
  Xjs <- datalist[[2]]
  beta <- datalist[[3]]
  betajs <- datalist[[4]]
  grptrack <- datalist[[5]]
  jnorm <- datalist[[6]]
  orthX <- datalist[[7]]
  xstar <- datalist[[8]]
  
  
  popcov <- cov(xstar)*(NROW(xstar)-1)/NROW(xstar)
  
  if(is.null(lambda)){ ####find better lambda selection
    lambda <- exp(seq(log(lambda.max),log(lambda.min),len=n.lambda))
  }
  
  bic <- numeric(length(lambda))
  iter <- numeric(length(lambda))
  betafinal <- matrix(0, ncol = sum(sapply(betajs, length)), nrow = length(lambda))
  initbeta <- beta
  initbetajs <- betajs
  if(penalty == "grMCP" & gamma<=1){stop("gamma must be > 1 for MCP")}
  if(penalty == "SCAD" & gamma<=2){stop("gamma must be > 2 for SCAD")}
  
  if(penalty == "grLasso"){thresh_fun <- grp_S_thresh}else if(penalty == "grMCP"){thresh_fun <- grp_F}else if(penalty == "grSCAD"){thresh_fun <- grp_Fs}
  for(i in 1:length(lambda)){
  ############################# Starting at solution of previous lambda leads to different solutions (non-unique solution? lasso is convex)
      #if(i == 1){
       # initbeta <- beta
        #initbetajs <- betajs
      #}else{
       # for(j in 1:length(betajs)){
        #  initbeta[which(grptrack == unique(grptrack)[j])] <- finalbetajs[[j]]
         # initbetajs <- finalbetajs
    #    }
     # }
    fit <- descent_alg(ystar, orthX, initbeta, Xjs, betajs, lambda[i], gamma, thresh_fun)
    finalbetajs <- fit[[1]]
    iter[i] <- fit[[2]]
    for(j in 1:length(finalbetajs)){
      if(length(which(grptrack == unique(grptrack)[j]))>1){
        betafinal[i,which(grptrack == unique(grptrack)[j])] <- beta_denorm(finalbetajs[[j]], jnorm[[j]]$Q, jnorm[[j]]$invrtlam)
      }else{
        betafinal[i,which(grptrack == unique(grptrack)[j])] <- finalbetajs[[j]]*jnorm[[j]]
      }
    }
    bic[i] <- biccheck(betafinal[i,], YS, XS, YO, XO)
      
    #transforming rho
    betafinal[i,dim(betafinal)[2]] <- tanh(betafinal[i,dim(betafinal)[2]])
    
  }
  
  
  colnames(betafinal) <- rep(NA, dim(betafinal)[2])
  for(i in 1:length(betafinal)){
    if(i<=dim(XS)[2]){
      colnames(betafinal)[i] <- paste("S", colnames(XS)[i], sep = ":")
    }else if(dim(XS)[2]< i & dim(betafinal)[2] > i){
      colnames(betafinal)[i] <- paste("Y", colnames(XO)[i-dim(XS)[2]], sep = ":")
    }else if(i == dim(betafinal)[2]){
      colnames(betafinal)[i] <- "rho"
    }
  }
  
  
  best <- which.min(bic)
  min.bic <- bic[best]
  best_beta <- betafinal[best,]
  best_lambda <- lambda[best]
  best.iter <- iter[best]
  
  
  
  if(full == TRUE){
    output <- list("parameters" = betafinal, "lambda" = lambda, "bic" = bic, "iterations" = iter, "Popcov" = popcov)
    return(output)
  }else{
    output <- list("best model" = best_beta, "best lambda" = best_lambda, "bic" = min.bic, "iter" = best.iter, "Popcov" = popcov)
    return(output)
  }
  
}

#YS XS YO XO need to be globally declared for some reason!!!!!!!!!
# sim <- function(n, rho, penalty){
#   sim_lambda <- rep(0, 200)
#   MSE <- rep(0, 200)
#   Cg <- rep(0, 200)
#   ICg <- rep(0, 200)
#   indcor <- rep(0, 200)
#   for(i in 1:200){
#     my_data <- grp_gendat_bin(i, n, rho, 5,4)
#     mf <- model.frame(selection, my_data)
#     YS <- model.response(mf, "numeric")
#     XS <- model.matrix(selection, data = my_data)
#     mf2 <- model.frame(outcome, my_data)
#     YO <- model.response(mf2, "numeric")
#     XO <- model.matrix(outcome, data = my_data)
#     if(penalty == "grSCAD"){gamma <- 3.7}else if(penalty == "grMCP"){gamma <- 3}else{gamma <- 0}
#     
#     beta <- lambda_select(YS, XS, YO, XO, group, penalty, gamma, lambda.min = 0.01, n.lambda = 100,lambda.max = 0.5)
#     sim_lambda[i] <- beta$`best lambda`
#     for(j in c(10, 18, 39, 35)){
#       if(beta$`best model`[j] == 0){
#         Cg[i] = Cg[i]+1
#       }
#     }
#     for(k in c(2, 6, 14, 23, 27, 31)){
#       if(beta$`best model`[k] == 0){
#         ICg[i] = ICg[i]+1
#       }
#     }
#     if(Cg[i] == 4 & ICg[i] == 0){indcor[i] <- 1}
#     MSE[i] <- t(beta$`best lambda`-beta_true)%*%beta$Popcov%*%(beta$`best lambda`-beta_true)
#     print(beta$`best model`)
#   }
#   return(list("Penalty" = penalty, "Sample size" = n, "Cg" = mean(Cg), "ICg" = mean(ICg), "pcorr" = length(which(indcor != 0))/length(indcor)*100, "MMSE" = median(MSE), "lambda mean" = mean(sim_lambda), "lambda sd" = sd(sim_lambda)))
# }





###########group reg Simulation

selection <- uu ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11+ X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20
outcome <- yobs ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11+ X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20
group <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5)
#group <- 1:20
names(group) <- c("X1", "X2", "X3","X4", "X5", "X6","X7", "X8", "X9", "X10", "X11", "X12", "X13","X14", "X15", "X16","X17", "X18", "X19", "X20")

results <- as.data.frame(matrix(rep(NA, 216), ncol = 12))
names(results) <- c("rho", "Sample size", "Penalty", "Cg", "ICg", "pcorr", "MMSE", "bic", "lambda mean", "lambda sd" , "nmax", "nmin")

t1 <- Sys.time()
for(a in 1:18){
  sim_lambda <- rep(0, 200)
  MSE <- rep(0, 200)
  Cg <- rep(0, 200)
  ICg <- rep(0, 200)
  indcor <- rep(0, 200)
  
  
  if(a %in% 1:6){rho <- 0.2}else if(a %in% 7:12){rho <- 0.5}else if(a %in% 13:18){rho <- 0.8}
  if(a %in% c(1,2,3,7,8,9,13,14,15)){
    n <-  1000
  }else{n <- 400}
  if(a%%3 == 1){penalty <- "grLasso"}else if(a %% 3 == 2){penalty <- "grMCP"}else if(a %% 3 == 0){penalty <- "grSCAD"}

  beta_true <- c(1.24, -0.6, -0.3, 0.3, 0.6, -0.5, -0.2, 0.2, 0.5, 0, 0, 0, 0, 0.15, 0.15, 0.15, 0.15, 0, 0, 0, 0, -1.8, -1, -0.25, 0.25, 1,-0.5, -0.1, 0.1, 0.5, 0.2, 0.2, 0.2, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, rho)
  
  for(i in 1:200){
    my_data <- grp_gendat_bin(i, n, rho, 5,4)
    mf <- model.frame(selection, my_data)
    YS <- model.response(mf, "numeric")
    XS <- model.matrix(selection, data = my_data)
    mf2 <- model.frame(outcome, my_data)
    YO <- model.response(mf2, "numeric")
    XO <- model.matrix(outcome, data = my_data)
    if(penalty == "grSCAD"){gamma <- 3.7}else if(penalty == "grMCP"){gamma <- 3}else{gamma <- 0}

    beta <- lambda_select(YS, XS, YO, XO, group, penalty, gamma, lambda.min = 0.01, n.lambda = 100,lambda.max = 0.4)
    sim_lambda[i] <- beta$`best lambda`
    for(j in c(10, 18, 39, 35)){
      if(beta$`best model`[j] == 0){
        Cg[i] = Cg[i]+1
      }
    }
    for(k in c(2, 6, 14, 23, 27, 31)){
      if(beta$`best model`[k] == 0){
        ICg[i] = ICg[i]+1
      }
    }
    if(Cg[i] == 4 & ICg[i] == 0){indcor[i] <- 1}
    MSE[i] <- t(beta$`best model`-beta_true)%*%beta$Popcov%*%(beta$`best model`-beta_true)
  }
  results[a,] <- list("rho" = rho, "Sample size" = n, "Penalty" = penalty, "Cg" = mean(Cg), "ICg" = mean(ICg), "pcorr" = length(which(indcor != 0))/length(indcor)*100, "MMSE" = median(MSE), "bic" = beta$bic, "lambda mean" = mean(sim_lambda), "lambda sd" = sd(sim_lambda), "nmax" = length(which(sim_lambda == 0.4)), "nmin" = length(which(sim_lambda == 0.01)))

}
t2 <- Sys.time()

t2-t1

results

results1

results1 <- results

write.csv(results, "C:\\Users\\Henry Jones\\OneDrive\\Documents\\Durham Maths work\\Fourth year\\Project IV\\Simulation results\\grp_sim_full.csv")




####################Group sim
####################


selection <- uu ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11+ X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20
outcome <- yobs ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11+ X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20
group <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5)
#group <- 1:20
names(group) <- c("X1", "X2", "X3","X4", "X5", "X6","X7", "X8", "X9", "X10", "X11", "X12", "X13","X14", "X15", "X16","X17", "X18", "X19", "X20")

results <- as.data.frame(matrix(rep(NA, 18), ncol = 3))
names(results) <- c("Sample size", "MMSE", "rho")

penalty <- "grLasso"

t1 <- Sys.time()
for(a in 1:6){
  MSE <- rep(0, 200)
  if(a %in% c(2,4,6)){
    n <-  1000
  }else if(a %in% c(1,3,5)){n <- 400}
  if(a %in% c(1,2)){rho <- 0.2}else if(a %in% c(3,4)){rho <- 0.5}else if(a %in% c(5,6)){rho <- 0.8}
  beta_true <- c(1.24, -0.6, -0.3, 0.3, 0.6, -0.5, -0.2, 0.2, 0.5, 0, 0, 0, 0, 0.15, 0.15, 0.15, 0.15, 0, 0, 0, 0, -1.8, -1, -0.25, 0.25, 1,-0.5, -0.1, 0.1, 0.5, 0.2, 0.2, 0.2, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, rho)
  
  for(i in 1:2000){
    my_data <- grp_gendat_bin(i, n, rho, 5,4)
    mf <- model.frame(selection, my_data)
    YS <- model.response(mf, "numeric")
    XS <- model.matrix(selection, data = my_data)
    mf2 <- model.frame(outcome, my_data)
    YO <- model.response(mf2, "numeric")
    XO <- model.matrix(outcome, data = my_data)
    
    
    beta <- lambda_select(YS, XS, YO, XO, group, penalty, gamma = 0, lambda = 0)
    
    MSE[i] <- t(beta$`best model`-beta_true)%*%beta$Popcov%*%(beta$`best model`-beta_true)
  }
  results[a,] <- list("Sample size" = n, "MMSE" = median(MSE), "rho" = rho)
  
}
t2 <- Sys.time()

t2-t1

results

write.csv(results, "C:\\Users\\Henry Jones\\OneDrive\\Documents\\Durham Maths work\\Fourth year\\Project IV\\Simulation results\\grp_lambda0.csv")



###########liner reg test

for(a in 1:6){
  MSE <- rep(0, 200)
  if(a %in% c(2,4,6)){
    n <-  1000
  }else if(a %in% c(1,3,5)){n <- 400}
  if(a %in% c(1,2)){rho <- 0.2}else if(a %in% c(3,4)){rho <- 0.5}else if(a %in% c(5,6)){rho <- 0.8}
  beta_true <- c(1.24, -0.6, -0.3, 0.3, 0.6, -0.5, -0.2, 0.2, 0.5, 0, 0, 0, 0, 0.15, 0.15, 0.15, 0.15, 0, 0, 0, 0, -1.8, -1, -0.25, 0.25, 1,-0.5, -0.1, 0.1, 0.5, 0.2, 0.2, 0.2, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, rho)
  
  for(i in 1:2000){
    my_data <- grp_gendat_bin(i, n, rho, 5,4)
    mf <- model.frame(selection, my_data)
    YS <- model.response(mf, "numeric")
    XS <- model.matrix(selection, data = my_data)
    mf2 <- model.frame(outcome, my_data)
    YO <- model.response(mf2, "numeric")
    XO <- model.matrix(outcome, data = my_data)
    
    

    MSE[i] <- t(beta$`best model`-beta_true)%*%beta$Popcov%*%(beta$`best model`-beta_true)
  }
  results[a,] <- list("Sample size" = n, "MMSE" = median(MSE), "rho" = rho)
  
}
t2 <- Sys.time()







###########std reg Simulation

selection <- uu ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11+ X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20
outcome <- yobs ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11+ X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20
#group <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5)
group <- 1:20
names(group) <- c("X1", "X2", "X3","X4", "X5", "X6","X7", "X8", "X9", "X10", "X11", "X12", "X13","X14", "X15", "X16","X17", "X18", "X19", "X20")

results <- as.data.frame(matrix(rep(NA, 216), ncol = 12))
names(results) <- c("rho", "Sample size", "Penalty", "C", "IC", "pcorr", "MMSE", "bic", "lambda mean", "lambda sd" , "nmax", "nmin")

t1 <- Sys.time()
for(a in 1:18){
  sim_lambda <- rep(0, 200)
  MSE <- rep(0, 200)
  C <- rep(0, 200)
  IC <- rep(0, 200)
  indcor <- rep(0, 200)
  
  
  if(a %in% 1:6){rho <- 0.2}else if(a %in% 7:12){rho <- 0.5}else if(a %in% 13:18){rho <- 0.8}
  if(a %in% c(1,2,3,7,8,9,13,14,15)){
    n <-  1000
  }else{n <- 400}
  if(a%%3 == 1){penalty <- "grLasso"}else if(a %% 3 == 2){penalty <- "grMCP"}else if(a %% 3 == 0){penalty <- "grSCAD"}
  
  beta_true <- c(1.24, -0.6, -0.3, 0.3, 0.6, -0.5, -0.2, 0.2, 0.5, 0, 0, 0, 0, 0.15, 0.15, 0.15, 0.15, 0, 0, 0, 0, -1.8, -1, -0.25, 0.25, 1,-0.5, -0.1, 0.1, 0.5, 0.2, 0.2, 0.2, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, rho)
  
  for(i in 1:200){
    my_data <- grp_gendat_bin(i, n, rho, 5,4)
    mf <- model.frame(selection, my_data)
    YS <- model.response(mf, "numeric")
    XS <- model.matrix(selection, data = my_data)
    mf2 <- model.frame(outcome, my_data)
    YO <- model.response(mf2, "numeric")
    XO <- model.matrix(outcome, data = my_data)
    if(penalty == "grSCAD"){gamma <- 3.7}else if(penalty == "grMCP"){gamma <- 3}else{gamma <- 0}
    
    beta <- lambda_select(YS, XS, YO, XO, group, penalty, gamma, lambda.min = 0.01, n.lambda = 100,lambda.max = 0.4)
    sim_lambda[i] <- beta$`best lambda`
    for(j in c(10, 11, 12, 13, 18, 19, 20, 21, 39, 40, 41, 42, 35, 36, 37, 38)){
      if(beta$`best model`[j] == 0){
        C[i] = C[i]+1
      }
    }
    for(k in c(2, 3, 4, 5, 6, 7, 8, 9, 14, 15, 16, 17, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34)){
      if(beta$`best model`[k] == 0){
        IC[i] = IC[i]+1
      }
    }
    if(C[i] == 16 & IC[i] == 0){indcor[i] <- 1}
    MSE[i] <- t(beta$`best model`-beta_true)%*%beta$Popcov%*%(beta$`best model`-beta_true)
  }
  results[a,] <- list("rho" = rho, "Sample size" = n, "Penalty" = penalty, "C" = mean(C), "IC" = mean(IC), "pcorr" = length(which(indcor != 0))/length(indcor)*100, "MMSE" = median(MSE), "bic" = beta$bic, "lambda mean" = mean(sim_lambda), "lambda sd" = sd(sim_lambda), "nmax" = length(which(sim_lambda == 0.4)), "nmin" = length(which(sim_lambda == 0.01)))
  
}
t2 <- Sys.time()

t2-t1

results

results1 <- results


write.csv(results, "C:\\Users\\Henry Jones\\OneDrive\\Documents\\Durham Maths work\\Fourth year\\Project IV\\Simulation results\\stdreg.csv")





##############################               ##############################   
############################## Data Analysis ##############################
##############################               ##############################

hiv_data <- read.csv("C:\\Users\\Henry Jones\\OneDrive\\Documents\\Durham Maths work\\Fourth year\\Project IV\\HIV Data\\Final_ZM_data_reduced.csv")

hiv_data <- hiv_data[,-1]

hiv_data$hiv[which(is.na(hiv_data$hiv))] <- 0

names(hiv_data)

# selection <- hivconsent ~ interviewerID_103 + interviewerID_104 + interviewerID_105 + interviewerID_106 + 
# interviewerID_107 + interviewerID_108 + interviewerID_213 + interviewerID_214 + interviewerID_215 + 
#   interviewerID_216 + interviewerID_217 + interviewerID_218 + interviewerID_223 + interviewerID_224 +
#   interviewerID_225 + interviewerID_226 + interviewerID_227 + interviewerID_228 + interviewerID_303 +
#   interviewerID_304 + interviewerID_305 + interviewerID_306 + interviewerID_307 + interviewerID_308 +
#   interviewerID_403 + interviewerID_404 + interviewerID_405 + interviewerID_406 + interviewerID_407 +
#   interviewerID_408 + interviewerID_513 + interviewerID_514 + interviewerID_515 + interviewerID_516 +
#   interviewerID_517 + interviewerID_518 + interviewerID_523 + interviewerID_524 + interviewerID_525 +
#   interviewerID_526 + interviewerID_527 + interviewerID_528 + interviewerID_613 + interviewerID_614 +
#   interviewerID_615 + interviewerID_616 + interviewerID_617 + interviewerID_618 + interviewerID_623 +
#   interviewerID_624 + interviewerID_625 + interviewerID_626 + interviewerID_627 + interviewerID_628 +
#   interviewerID_703 + interviewerID_704 + interviewerID_705 + interviewerID_706 + interviewerID_707 +
#   interviewerID_708 + interviewerID_803 + interviewerID_804 + interviewerID_805 + interviewerID_806 +
#   interviewerID_807 + interviewerID_808 + interviewerID_903 + interviewerID_904 + interviewerID_905 +
#   interviewerID_906 + interviewerID_907 + interviewerID_908 + agecat5_20.24 + agecat5_25.29 + agecat5_30.34 +
#   agecat5_35.39 + agecat5_40.44 + agecat5_45.49 + agecat5_50.54 + agecat5_55.60 + wealthcat_Poorest +
#   wealthcat_Poorer + wealthcat_Middle + wealthcat_Richest + location_Capital..large.city + location_Small.city +
#   location_Town + region_Central + region_Copperbelt + region_Luapula + region_Lusaka + region_Northern +
#   region_Northwestern + region_Southern + region_Western + marital_Never.married + marital_Formerly.married +
#   age1sex_cat_nosex + age1sex_cat_under16 + partner_nopartner + partner_2.partner + religion_Catholic +
#   religion_Protestant + ethnicity_Bemba + ethnicity_Lunda..Luapula. + ethnicity_Lala +ethnicity_Ushi +
#   ethnicity_Lamba + ethnicity_Tonga + ethnicity_Luvale + ethnicity_Lunda..Northwestern. + ethnicity_Mbunda +
#   ethnicity_Kaonde + ethnicity_Lozi + ethnicity_Chewa + ethnicity_Nsenga + ethnicity_Ngoni + ethnicity_Mambwe +
#   ethnicity_Namwanga + ethnicity_Tumbuka + language_English + language_Bemba + language_Lozi + language_Nyanja +
#   language_Tonga + sex + education + std + highhiv + condom + aidscare + knowsdiedofaids +
#   evertestedHIV + smoke + alcohol

outcome = hiv ~ agecat5_20.24 + agecat5_25.29 + agecat5_30.34 +
  agecat5_35.39 + agecat5_40.44 + agecat5_45.49 + agecat5_50.54 + agecat5_55.60 + wealthcat_Poorest +
  wealthcat_Poorer + wealthcat_Middle + wealthcat_Richest + location_Capital..large.city + location_Small.city +
  location_Town + region_Central + region_Copperbelt + region_Luapula + region_Lusaka + region_Northern + 
  region_Northwestern + region_Southern + region_Western + marital_Never.married + marital_Formerly.married +
  age1sex_cat_nosex + age1sex_cat_under16 + partner_nopartner + partner_2.partner + religion_Catholic + 
  religion_Protestant + ethnicity_Bemba + ethnicity_Lunda..Luapula. + ethnicity_Lala +ethnicity_Ushi +
  ethnicity_Lamba + ethnicity_Tonga + ethnicity_Luvale + ethnicity_Lunda..Northwestern. + ethnicity_Mbunda +
  ethnicity_Kaonde + ethnicity_Lozi + ethnicity_Chewa + ethnicity_Nsenga + ethnicity_Ngoni + ethnicity_Mambwe + 
  ethnicity_Namwanga + ethnicity_Tumbuka + language_English + language_Bemba + language_Lozi + language_Nyanja +
  language_Tonga + sex + education + std + highhiv + condom + aidscare + knowsdiedofaids + 
  evertestedHIV + smoke + alcohol

selection <- hivconsent ~  agecat5_20.24 + agecat5_25.29 + agecat5_30.34 +
  agecat5_35.39 + agecat5_40.44 + agecat5_45.49 + agecat5_50.54 + agecat5_55.60 + wealthcat_Poorest +
  wealthcat_Poorer + wealthcat_Middle + wealthcat_Richest + location_Capital..large.city + location_Small.city +
  location_Town + region_Central + region_Copperbelt + region_Luapula + region_Lusaka + region_Northern +
  region_Northwestern + region_Southern + region_Western + marital_Never.married + marital_Formerly.married +
  age1sex_cat_nosex + age1sex_cat_under16 + partner_nopartner + partner_2.partner + religion_Catholic +
  religion_Protestant + ethnicity_Bemba + ethnicity_Lunda..Luapula. + ethnicity_Lala +ethnicity_Ushi +
  ethnicity_Lamba + ethnicity_Tonga + ethnicity_Luvale + ethnicity_Lunda..Northwestern. + ethnicity_Mbunda +
  ethnicity_Kaonde + ethnicity_Lozi + ethnicity_Chewa + ethnicity_Nsenga + ethnicity_Ngoni + ethnicity_Mambwe +
  ethnicity_Namwanga + ethnicity_Tumbuka + language_English + language_Bemba + language_Lozi + language_Nyanja +
  language_Tonga + sex + education + std + highhiv + condom + aidscare + knowsdiedofaids +
  evertestedHIV + smoke + alcohol



mf<- model.frame(selection, hiv_data)
YS <- model.response(mf, "numeric")
XS <- model.matrix(selection, data = hiv_data)
mf2 <- model.frame(outcome, hiv_data)
YO <- model.response(mf2, "numeric")
XO <- model.matrix(outcome, data = hiv_data)

group <- c(rep(1, 72), rep(2, 8), rep(3, 4), rep(4, 3), rep(5, 8), rep(6,2), rep(7,2), rep(8,2), rep(9,2), rep(10,17), 
           rep(11,5), 12, NA, NA, 13, 14, 15, 16, 17, 18, 19, 20, 21)

names(group) <- names(hiv_data)

grlasso <- lambda_select(YS, XS, YO, XO, group, penalty = "grLasso", gamma = 0, lambda.min = 0.01, n.lambda = 100,lambda.max = 0.3, full = TRUE)

bestl <- which.min(grlasso$bic)
grlasso$parameters[bestl,]



grMCP <- lambda_select(YS, XS, YO, XO, group, penalty = "grMCP", gamma = 3, lambda.min = 0.01, n.lambda = 100,lambda.max = 0.3, full = TRUE)

bestm <- which.min(grMCP$bic)
grMCP$parameters[bestm,]



grSCAD <- lambda_select(YS, XS, YO, XO, group, penalty = "grSCAD", gamma = 3.7, lambda.min = 0.01, n.lambda = 100,lambda.max = 0.3, full = TRUE)

bests <- which.min(grSCAD$bic)
grSCAD$parameters[bests,]


par(mfrow = c(1,3))
plot(grlasso$lambda,grlasso$bic)
plot(grMCP$lambda,grMCP$bic)
plot(grSCAD$lambda,grSCAD$bic)



model0 <- lambda_select(YS, XS, YO, XO, group, penalty = "grLasso", gamma = 0, lambda = 0, full = TRUE)

model0$parameters
model0$bic

grlasso$bic[bestl]

model0$parameters

length(model0$parameters)

models <- as.data.frame(matrix(NA, ncol = 3, nrow = 131))
colnames(models) <- c("grLasso", "grMCP", "grSCAD")

models[1,] <- c(model0$lambda, grMCP$lambda[bestm], grSCAD$lambda[bests])
models[2,] <- c(model0$bic, grMCP$bic[bestm], grSCAD$bic[bests])
models[3:131,1] <- t(model0$parameters)
models[3:131,2] <- grMCP$parameters[bestm,]
models[3:131,3] <- grSCAD$parameters[bests,]

rownames(models) <- c("lambda", "bic", colnames(model0$parameters))

write.csv(models, "C:\\Users\\Henry Jones\\OneDrive\\Documents\\Durham Maths work\\Fourth year\\Project IV\\HIV Data\\hivanalysis.csv")





###########Test area############
my_data <- grp_gendat_bin(1, 1000, 0.5, 5,4)

#length(my_data$uu[my_data$uu == 1])/length(my_data$uu)
#length(my_data$yy[my_data$yy== 1 & my_data$uu == 1])/length(my_data$uu == 1)

selection <- uu ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11+ X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20
outcome <- yobs ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11+ X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20

group <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5)
names(group) <- c("X1", "X2", "X3","X4", "X5", "X6","X7", "X8", "X9", "X10", "X11", "X12", "X13","X14", "X15", "X16","X17", "X18", "X19", "X20")
#lambda <- 0

t1 <- Sys.time()
mf <- model.frame(selection, my_data)
YS <- model.response(mf, "numeric")
XS <- model.matrix(selection, data = my_data)

mf2 <- model.frame(outcome, my_data)
YO <- model.response(mf2, "numeric")
XO <- model.matrix(outcome, data = my_data)



beta <- lambda_select(YS, XS, YO, XO, group, "grLasso", gamma = 0, lambda.min = 0.01, n.lambda = 100,lambda.max = 0.5)
t2 <- Sys.time()
t2-t1

beta <- lambda_select(YS, XS, YO, XO, group, "grLasso", gamma = 0,lambda.min = 0.08, n.lambda = 100,lambda.max = 0.3,full = TRUE)


plot(beta$bic)
which.min(beta$bic)

beta1 <- lambda_select(YS, XS, YO, XO, group, "grLasso", gamma = 3,lambda = 0.3)

beta2 <- lambda_select(YS, XS, YO, XO, group, "grSCAD", gamma = 3,lambda = 0.3)


beta1$`best model` - beta2$`best model`


