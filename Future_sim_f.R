mperf     = function(sim,obs,vnam,dchart,outidx){
  
  #--- Compute statistical indexes of performance
  #--- Performance function
  # sim     - simulated values [R]
  # obs     - observed values [R]
  # vnam    - Name of variable as string for chart axis  [S]
  # dchart  - Display Chart? [T or F]
  
  #--- Statistical indexes
  fit   = lm(sim~obs)
  bias  = (1/length(obs)) * sum(sim-obs)
  mse   = (1/length(obs)) * sum((sim-obs)^2)
  rmse  = sqrt(mse)
  mae   = (1/length(obs)) * sum(abs(sim-obs))
  rrmse = rmse / mean(obs)
  rmae  = (1/length(obs[obs>0])) * sum(abs(sim[obs>0]-obs[obs>0])/abs(obs[obs>0]))
  ef    = 1 - (sum((sim-obs)^2) / sum((obs-mean(obs))^2))
  r     = sum((obs-mean(obs))*(sim-mean(sim)))/sqrt(sum((obs-mean(obs))^2)*sum((sim-mean(sim))^2))
  r2    = r^2
  d     = 1 - (sum((sim-obs)^2) / sum((abs(sim-mean(obs))+abs(obs-mean(obs)))^2))
  #if(length(unique(sim)) > 1){
  #  a     = summary(fit)$coefficients["(Intercept)","Estimate"]
  #  b     = summary(fit)$coefficients["obs","Estimate"]
  #}
  
  if(dchart){
    #--- Chart Sim ~ Obs
    varlab = vnam 
    
    mindt = min(obs,sim)
    maxdt = max(obs,sim)
    #--- Ploting limits 
    pllim = c(mindt-0.1*(maxdt-mindt),maxdt+0.1*(maxdt-mindt))
    xx = seq(min(obs),max(obs),length = (max(obs)-min(obs))*1000)
    #z = summary(fit)
    
    plot(sim~obs,
         ylab = paste("Sim - ",varlab,sep = ""),
         xlab = paste("Obs - ",varlab,sep = ""),
         ylim = pllim,
         xlim = pllim)
    
    lines(xx, predict(fit, data.frame(obs=xx)),
          col = "black",
          lty = 1,
          lwd = 1.5)
    
    l11 = seq(pllim[1]-0.5*(maxdt-mindt), pllim[2] + 0.5 * (maxdt-mindt),length = 1000)
    
    lines(l11*1~l11,
          col = "red",
          lty = 2,
          lwd = 1.5)
  }
  
  
  perf = data.frame(vnam,
                    bias,
                    mse,
                    rmse,
                    mae,
                    rrmse,
                    rmae,
                    ef,
                    r,
                    r2,
                    d)
  
  perf[,c("vnam",outidx)]
  
}


YYDOY = function(dat,todssat,century){
  #---------------------------------------------------------#
  #--- Function to convert regular dates in DSSAT format ---#
  #---------------------------------------------------------#
  #--- Murilo Vianna, Jul-2018
  #--- Usage: 
  #---    dat:        The data to be converted (single or vector) in the R Date format YYYY-MM-DD or YYDOY (DSSAT)
  #---    todssat:    A logical. If true the convertion YYYY-MM-DD -> YYDOY will be the output, if false YYDOY -> YYYY-MM-DD
  #---    century:    If todssat = f: specify the century of data (e.g. 1900, 2000), this is a limitation of DSSAT format
  #---------------------------------------------------------#
  
  if(missing(todssat)){todssat=T}
  
  if(todssat){
    #--- Test data
    if(typeof(dat) == "character" & nchar(dat[1]) > 7){
      #--- convert dat to date if not done before
      dat = as.Date(dat)
    }
    #--- dat still not as date stop
    if(typeof(dat) != "double"){stop("Input data is not in Date format (YYYY-MM-DD). Please check dat format.")}
    
    #--- Converts from YYYY-MM-DD to YYDOY format
    year = format(as.Date(dat), "%Y")
    doy  = as.Date(dat) - as.Date(paste0(year,"-01-01")) + 1  
    
    #--- Re-build DSSAT DATE format (YYDOY)
    return (paste0(substr(year,3,4),sprintf("%003.0f",doy)))    
  }else{
    
    #--- check if century is set otherwise default=2000
    if(missing(century)){century=2000}
    
    #--- Test data
    if(typeof(dat) != "character" | nchar(dat[1]) != 5){stop("Input data is not in DSSAT Date format (YYDOY). Please check dat format.")}
    
    #--- passing data
    doy = as.numeric(substr(dat,3,5))
    year= as.numeric(substr(dat,1,2))
    
    
    #--- Re-build DATE format (YYYY-MM-DD)
    return(as.Date(paste0(century+year,"-01-01")) + (doy - 1)) 
    
  }
}