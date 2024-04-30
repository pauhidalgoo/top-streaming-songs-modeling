# LDA, realitzar clústering
library(topicmodels)

ap_lda <- LDA(AssociatedPress, k=10, control = list(seed=1234))
ap_lda

ap_topics <- tidy(ap_lda, matrix = "beta")

library(tidytext)