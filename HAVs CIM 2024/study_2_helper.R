lib_names <- c("semTools", "vroom", "tidyverse", "manymome", "rstatix")
lapply(lib_names, library, character.only = T)

# All items
m_1 <- "PUB =~ cim_public_1+cim_public_2+cim_public_3
LOC=~ cim_local_1+cim_local_2+cim_local_3
PF =~ par_fear_1+par_fear_2+par_fear_3+par_fear_4
FoS =~ stranger_fear_1+stranger_fear_2+stranger_fear_3+stranger_fear_4+stranger_fear_5
PPPOA =~ potentiality1+potentiality2+potentiality3+potentiality4+potentiality5+potentiality6+potentiality7
PI =~ par_int_1+par_int_2+par_int_3+par_int_4+par_int_5
"
# Drop potentiality6 for low loadings
m_2 <- "PUB =~ cim_public_1+cim_public_2+cim_public_3
LOC=~ cim_local_1+cim_local_2+cim_local_3
PF =~ par_fear_1+par_fear_2+par_fear_3+par_fear_4
FoS =~ stranger_fear_1+stranger_fear_2+stranger_fear_3+stranger_fear_4+stranger_fear_5
PPPOA =~ potentiality1+potentiality2+potentiality3+potentiality4+potentiality5+potentiality7
PI =~ par_int_1+par_int_2+par_int_3+par_int_4+par_int_5
"
# Remove public2
m_3 <- "PUB =~ cim_public_1+cim_public_3
LOC=~ cim_local_1+cim_local_2+cim_local_3
PF =~ par_fear_1 + par_fear_2+par_fear_3+par_fear_4
FoS =~ stranger_fear_1+stranger_fear_2+stranger_fear_3+stranger_fear_4+stranger_fear_5
PPPOA =~ potentiality1+potentiality2++potentiality3+potentiality4+potentiality5+potentiality7
PI =~ par_int_1+par_int_2+par_int_3+par_int_4+par_int_5
"
# Remove local2 and potentiality3 for problematic residuals
m_4 <- "PUB =~ cim_public_1+cim_public_3
LOC=~ cim_local_1+cim_local_3
PF =~ par_fear_1 + par_fear_2+par_fear_3+par_fear_4
FoS =~ stranger_fear_1+stranger_fear_2+stranger_fear_3+stranger_fear_4+stranger_fear_5
PPPOA =~ potentiality1+potentiality2+potentiality4+potentiality5+potentiality7
PI =~ par_int_1+par_int_2+par_int_3+par_int_4+par_int_5
"


# Structural Model
m_5 <- "
PUB =~ cim_public_1+cim_public_3
LOC=~ cim_local_1+cim_local_3
PF =~ par_fear_1 + par_fear_2+par_fear_3+par_fear_4
FoS =~ stranger_fear_1+stranger_fear_2+stranger_fear_3+stranger_fear_4+stranger_fear_5
PPPOA =~ potentiality1+potentiality2+potentiality4+potentiality5+potentiality7
PI =~ par_int_1+par_int_2+par_int_3+par_int_4+par_int_5

PI ~~ PUB + LOC
PUB ~~ LOC

# a-paths
PF ~ a1*FoS
PPPOA ~ a2*FoS

# b-paths
PI ~ b1*PF
PI ~ b2*PPPOA

PUB ~ b3*PF
PUB ~ b4*PPPOA

LOC ~ b5*PF
LOC ~ b6*PPPOA

# c - direct effects
PI ~ c1*FoS
PUB ~ c2*FoS
LOC ~ c3*FoS

# indirect effects
ind_fos_pf_pi := a1*b1
ind_fos_pppoa_pi := a2*b2

ind_fos_pf_pub := a1*b3
ind_fos_pppoa_pub := a2*b4

ind_fos_pf_loc := a1*b5
ind_fos_pppoa_loc := a2*b6

# total effects
tot_fos_pf_pi := c1+(a1*b1)
tot_fos_pppoa_pi := c1+(a2*b2)

tot_fos_pf_pub := c2+(a1*b3)
tot_fos_pppoa_pub := c2+(a2*b4)

tot_fos_pf_loc := c3+(a1*b5)
tot_fos_pppoa_loc := c3+(a2*b6)

"
# Generator for residual correlations


k_local <- function(fit, minimum_value){
  minimum_value <- 0.1
  temp <- lavResiduals(fit)
  temp <- temp$cov
  temp <- round(temp,2)
  temp[upper.tri(temp)] <- NA
  diag(temp) <- NA
  temp <- cor_gather(temp)
  temp <- temp %>% filter(cor >= minimum_value | cor <= -minimum_value)
  temp <- temp %>% arrange(desc(abs(cor)), var1, desc(var2))
  #temp2 <- temp %>% group_by(var1) %>% summarise(mean_residual = mean(cor))
  #temp2 <- temp2[order(abs(temp2$mean_residual),decreasing = T),]
  #raw <- as.data.frame(temp)
  #items <- as.data.frame(temp2)
  #print(temp2, n = 20)
  print(temp, n= 20)
  mod <- modificationindices(fit, minimum.value = 10, sort. = T)
  print(mod)
  
  freq1 <- as.data.frame(summary(as.factor(temp$var1)))
  freq1$items <- row.names(freq1)
  colnames(freq1) <- c("frequency", "items")
  freq1 <- freq1[order(abs(freq1$frequency),decreasing = T),]
  
  freq2 <- as.data.frame(summary(as.factor(temp$var2)))
  freq2$items <- row.names(freq2)
  colnames(freq2) <- c("frequency", "items")
  freq2 <- freq2[order(abs(freq2$frequency),decreasing = T),]
  
  
  print(ggplot(data = freq1, aes(x=items, y=frequency)) + 
    geom_bar(stat="identity", colour="black", fill="white") + 
    xlab("") + ylab("") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_text(aes(label = signif(frequency)), nudge_y = 0.2))
  print(ggplot(data = freq2, aes(x=items, y=frequency)) + 
    geom_bar(stat="identity", colour="black", fill="white") + 
    xlab("") + ylab("") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_text(aes(label = signif(frequency)), nudge_y = 0.2))
}

k_residual <- function(fit, minimum_value){
  minimum_value <- 0.1
  temp <- lavResiduals(fit)
  temp <- temp$cov
  temp <- round(temp,2)
  temp[upper.tri(temp)] <- NA
  diag(temp) <- NA
  temp <- cor_gather(temp)
  
}




                                                