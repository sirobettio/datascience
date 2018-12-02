library(plyr)

setwd("C:/dev/data/kaggle/EloMerchantCategoryReccomendation")

# Read csv files
merchants_df <- read.csv("merchants.csv",sep = ",")
train_df <- read.csv("train.csv",sep = ",")
test_df <- read.csv("test.csv",sep = ",")
historical_transactions_df <- read.csv("historical_transactions.csv",sep = ",")
new_merchant_transactions_df <- read.csv("new_merchant_transactions.csv",sep = ",")
new_merchant_transactions_df$source <- "N"
head(new_merchant_transactions_df)

rbind()

historical_transactions_df$authorized <- ifelse(historical_transactions_df$authorized_flag == "Y",1,0)
new_merchant_transactions_df$authorized <- ifelse(new_merchant_transactions_df$authorized_flag == "Y",1,0)
head(historical_transactions_df)

tr_summary_df <- aggregate(historical_transactions_df$card_id, by = list(num_transactions = historical_transactions_df$card_id), FUN = count)

historical_transactions_df$source <- "H"

trn <- rbind(historical_transactions_df,new_merchant_transactions_df)

head(train_df)

hist(train_df$target)

summary(train_df$target)

card_id_num_transaction <- count(historical_transactions_df, "card_id")
head(card_id_num_transaction)
hist(card_id_num_transaction$freq)
summary(card_id_num_transaction$freq)

card_id_sum_purchased <- aggregate(purchase_amount ~ card_id, data = historical_transactions_df, sum)
head(card_id_sum_purchased)
hist(card_id_sum_purchased$purchase_amount)
summary(card_id_sum_purchased$purchase_amount)

card_id_count_authorized <- aggregate(historical_transactions_df$authorized_flag, by = list(card_id = historical_transactions_df$card_id), FUN = count)



trn_samples <- trn[runif(10000,1,nrow(trn)),]
write.csv(trn_samples, file = "sample_transactions.csv")

head(trn_samples)


