library(tidyverse)
library(corrplot)
retail <-read_csv('retail.csv',skip = 1,col_names = T) %>%
  select(-c(1,34,35,36))

retail <- retail %>%
  mutate_if(~any(str_detect(.x,pattern = "^\\(.*\\)$")),
            ~if_else(str_detect(.x,pattern = "^\\(.*\\)$"),
                     str_c("$-",str_sub(.x,3,-2)),
                     .x))  %>% 
  mutate_if(~all(str_detect(.x,pattern = "^\\$.*")), ~parse_number(.x))

all_column <- colnames(retail)[3:length(colnames(retail))]
colnames(retail)[3:12] <- str_c(all_column[1:10],"_0")

retail <- reshape(retail,
        direction = "long",
        varying=colnames(retail)[3:length(colnames(retail))],
        timevar = "group",
        times = c("Jeans - Colored denim","Jeans - Wide-leg","Jeans - High-rise"),
        v.names = all_column[1:10],
        sep = "_",
        idvar = "id") %>%
  select(-id)

row.names(retail) <- NULL
#
retail_selected <- retail %>%
  select_if(~ !all(sort(unique(.x)) %in% sort(c(0,1))))

corrplot(cor(retail_selected %>% select_if(~is.numeric(.x))),method = "number")
