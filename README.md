# ROC-curve
Plotting the ROC curve by fitting a bias-reduced binomial response. 
``` r{}
require(ROCR)
require(brglm)
require(ggplot2)
# By @biomedical_informatics Edris Sharif Ramani Nov 1, 2023
df <- data.frame(a = sample(1:25,400,replace = T),
                 b = runif(400)*3, 
                 c = sample(1:10,400,replace = T))
df <- cbind(df,type=ifelse((df$a+df$b+df$c)>=20,
                           "high", "low")) 
df$type <- factor(df$type)
index <- sample(1:nrow(df), size = .80 * nrow(df))
train <- df[index, ]
test <- df[-index, ]
model <- brglm(type ~.,data=train, 
            family = binomial(link = "logit"))
pred <- predict(model,test,type="response")
pred <- prediction(pred, test$type)
roc <- performance(pred, "tpr", "fpr")
res <- data.frame(x = roc@x.values[[1]],
                  y = roc@y.values[[1]])
ggplot(res, aes(x = x, y = y )) +
  geom_line(color = "blue") +
  labs(subtitle = "ROC curve", x = "False positive rate",
       y = "True positive rate") +
  geom_abline(intercept = 0) + 
  theme_classic() +
  theme(axis.text = element_text(size = 14 , colour = "black"),
        plot.subtitle = element_text(size = 14, colour = "black", hjust = 0.5, vjust = 0),
        axis.title.y = element_text(size = 14, angle = 90),
        axis.title.x = element_text(size = 16, angle = 00),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))


```
