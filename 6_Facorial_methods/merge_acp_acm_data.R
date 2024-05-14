# Merge ACP and ACM data to be used in the Neural Network

load("./6_Factorial_methods/acm_data.RData")
load("./6_Factorial_methods/acp_data.RData")

acm_data <- ACM_DATA_NAME
acp_data <- ACP_DATA_NAME

# Merge datasets
data <- cbind(acp_data, acm_data)

  
save(data, "./6_Factorial_methods/data_acp_acm_merged.RData")
