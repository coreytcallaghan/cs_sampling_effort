#=======Sample Completeness Curve=========#

sample_coverage = function(freq, q, datatype = c("abundance","incidence_freq")){
  
  if(datatype=="abundance"){
    freq = freq[freq>0]
    n = sum(freq)
    f1 = sum(freq==1)
    f2 = sum(freq==2)
    A = ifelse(f2>0,2*f2/((n-1)*f1+2*f2),ifelse(f1>0,2/((n-1)*(f1-1)+2),1))
    
    c_hat = function(q){
      if (q==0){
        
        S_obs = length(freq)
        # f0_hat = if ( f2 == 0 ){( (n-1)/n ) * ( f1*(f1-1)/2 )} else {( (n-1)/n ) * ( (f1^2) / (2*f2) )}
        # f0_hat_star = ceiling(f0_hat)
        # c_hat = S_obs / (S_obs + f0_hat_star)
        f0_hat = ifelse ( f2 == 0 ,( (n-1)/n ) * ( f1*(f1-1)/2 ), ( (n-1)/n ) * ( (f1^2) / (2*f2) ))
        c_hat = S_obs / (S_obs + f0_hat)
        return(c_hat)
        
      } else if (q==1){  
        
        c_hat = 1 - (f1/n)*(1-A)
        return(c_hat)
        
      } else if (q==2){
        
        x = freq[freq>=2]
        c_hat = 1 - (f1/n)*( (A*(1-A))/sum( x*(x-1) / (n*(n-1)) ) )
        return(c_hat)
        
      } else {
        
        r <- 0:(n-1)
        sort.data = sort(unique(freq))
        tab = table(freq)
        term = sapply(sort.data,function(z){
          k=0:(n-z)
          sum(choose(k-q,k)*exp(lchoose(n-k-1,z-1)-lchoose(n,z)))
        })
        lambda_hat =  sum(tab*term) + ifelse(f1==0|A==1,0,f1/n*(1-A)^(1-n)*(A^(q-1)-sum(choose(q-1,r)*(A-1)^r)))
        c_hat = 1 - ((f1/n)*(A^(q-1))*(1-A)/lambda_hat)
        return(c_hat)
        
      }
    }
  } else {
    
    t = freq[1]
    freq = freq[-1]; freq = freq[freq>0]
    u = sum(freq)
    Q1 = sum(freq==1)
    Q2 = sum(freq==2)
    B = ifelse(Q2>0,2*Q2/((t-1)*Q1+2*Q2),ifelse(Q1>0,2/((t-1)*(Q1-1)+2),1))
    
    c_hat = function(q){
      if (q==0){
        
        S_obs = length(freq)
        # Chao2 = S_obs + ceiling(if ( Q2 == 0 ){( (t-1)/t ) * ( Q1*(Q1-1)/2 )} else {( (t-1)/t ) * ( (Q1^2) / (2*Q2) )})
        Q0_hat = ifelse( Q2 == 0,( (t-1)/t ) * ( Q1*(Q1-1)/2 ), ( (t-1)/t ) * ( (Q1^2) / (2*Q2) ))
        c_hat = S_obs / (S_obs + Q0_hat)
        return(c_hat)
        
      } else if (q==1){  
        
        c_hat = 1 - (Q1/u)*(1-B)
        return(c_hat)
        
      } else if (q==2){
        
        x = freq[freq>=2]
        c_hat = 1 - (t-1)*Q1*( (B*(1-B))/sum( x*(x-1) ) )
        return(c_hat)
        
      } else {
        
        r <- 0:(t-1)
        sort.data = sort(unique(freq))
        tab = table(freq)
        term = sapply(sort.data,function(z){
          k=0:(t-z)
          sum(choose(k-q,k)*exp(lchoose(t-k-1,z-1)-lchoose(t,z)))
        })
        phi_hat = sum(tab*term) + ifelse(Q1==0|B==1,0,Q1/t*(1-B)^(1-t)*(B^(q-1)-sum(choose(q-1,r)*(B-1)^r)))
        c_hat = 1 - ((Q1/t)*(B^(q-1))*(1-B)/phi_hat)
        return(c_hat)
      }
    }
    
  }
  
  sapply(q,c_hat)
  
}

bootstrap_sample = function(freq, B, datatype = c("abundance","incidence_freq")){
  
  if(datatype=="abundance"){
    
    freq = freq[freq>0]
    n = sum(freq)
    f1 = sum(freq == 1)
    f2 = sum(freq == 2)
    
    S_obs = length(freq)
    f0_hat = if ( f2 == 0 ){( (n-1)/n ) * ( f1*(f1-1)/2 )} else {( (n-1)/n ) * ( (f1^2) / (2*f2) )}
    f0_hat_star = ceiling(f0_hat)
    S_hat_Chao1 = S_obs + f0_hat_star
    
    c_hat = if ( f2 != 0 ){ 1 - (f1/n)*((n-1)*f1/(((n-1)*f1)+2*f2))
      
    } else if (f1 != 0) { 1 - (f1/n)*((n-1)*(f1-1)/(((n-1)*(f1-1))+2*f2)) } else { 1 }
    
    lambda_hat = (1-c_hat) / sum((freq/n)*(1-freq/n)^n )
    p_i_hat_obs = (freq/n) * (1-lambda_hat* (1-freq/n)^n ) 
    p_i_hat_unobs = rep( (1-c_hat)/ f0_hat_star, f0_hat_star )
    
    bootstrap_population = c(p_i_hat_obs,p_i_hat_unobs)
    bootstrap_sample = rmultinom(n=B, size=n, prob=bootstrap_population)
    return(bootstrap_sample)
    
  } else {
    
    t = freq[1]
    freq = freq[-1]; freq = freq[freq>0]
    u = sum(freq)
    Q1 = sum(freq==1)
    Q2 = sum(freq==2)
    
    S_obs = length(freq)
    Q_0_hat = if ( Q2 == 0 ){( (t-1)/t ) * ( Q1*(Q1-1)/2 )} else {( (t-1)/t ) * ( (Q1^2) / (2*Q2) )}
    Q_0_hat_star = ceiling(Q_0_hat)
    
    c_hat = if ( Q2 > 0 ){ 1 - (Q1/u)*((t-1)*Q1/(((t-1)*Q1)+2*Q2))
      
    } else { 1 - (Q1/u)*((t-1)*(Q1-1)/(((t-1)*(Q1-1))+2)) }
    
    tau_hat = (u/t) * (1-c_hat) / sum((freq/t)*(1-freq/t)^t )
    pi_i_hat_obs = (freq/t) * (1-tau_hat* (1-freq/t)^t ) 
    pi_i_hat_unobs = rep( (u/t) * (1-c_hat)/ Q_0_hat_star, Q_0_hat_star )
    
    bootstrap_population = c(1,pi_i_hat_obs,pi_i_hat_unobs)
    bootstrap_sample = sapply(1:length(bootstrap_population), function(i) rbinom(n=B, size=t, prob=bootstrap_population[i]))
    bootstrap_sample = if(B==1) {as.matrix(bootstrap_sample)} else {t(bootstrap_sample)}
    return(bootstrap_sample)
    
  }
  
  
  
}

sc_profile.nose = function(freq, q, datatype = c("abundance","incidence_freq")) {
  
  data.frame(Order.q=q, Estimate=sample_coverage(freq, q, datatype))
  
}

sc_profile = function(freq, q, B, conf, datatype = c("abundance","incidence_freq")) {
  
  bootstrap_samples = bootstrap_sample(freq, B, datatype)
  sc_bs = sapply(1:B, function(i) sample_coverage(bootstrap_samples[,i], q, datatype))
  
  LCL = sample_coverage(freq, q, datatype) - qnorm(1-(1-conf)/2)*apply(sc_bs, 1, sd); LCL[LCL<0]=0
  UCL = sample_coverage(freq, q, datatype) + qnorm(1-(1-conf)/2)*apply(sc_bs, 1, sd); UCL[UCL>1]=1
  
  data.frame(Order.q=q, Estimate=sample_coverage(freq, q, datatype), s.e.=apply(sc_bs, 1,sd), LCL=LCL, UCL=UCL)
  
}

plot_sc_profile_nose <- function(data){
  
  ggplot(data, aes(x=Order.q, y=Estimate, colour=Community))+
    geom_line(size=1.2) +    
    labs(x="Order q", y="sample completeness")+
    theme(text=element_text(size=18), legend.position="bottom")
  
}

plot_sc_profile <- function(data){
  
  ggplot(data, aes(x=Order.q, y=Estimate, colour=Community))+
    labs(x="Order q", y="sample completeness")+
    theme(text=element_text(size=18))+
    geom_ribbon(aes(ymin=LCL, ymax=UCL, fill=Community, colour=NULL), alpha=0.4,show.legend = FALSE)+
    geom_line(size=1.1,show.legend = FALSE)
  
}