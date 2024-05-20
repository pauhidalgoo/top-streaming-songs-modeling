# Merge ACP and ACM data to be used in the Neural Network

load("./6_Facorial_methods/acm_data.RData")
load("./6_Facorial_methods/acp_data.RData")

acm_data <- data_acm
acp_data <- data_psi

data <- cbind(acp_data, acm_data)
  
save(data, file = "./6_Facorial_methods/data_merged_acp_acm.RData")

