
set.seed(1234)

corr1 <- list()
corr2 <- list()

for (i in 1:1000) {
  set.seed(i)
  
  r1 <- rnorm(1:1000)
  r2 <- rnorm(1:1000)
  
  df <- data.frame(r1 = r1, 
                   r2 = r2,
                   t = 1:1000) %>% 
    mutate(cs1 = cumsum(r1),
           cs2 = cumsum(r2))
  
  
  corr1[[i]] <- cor(df$cs1, df$cs2)
  corr2[[i]] <- cor(df$r1, df$r2)
}


df2 <- data.frame(corr1 = unlist(corr1),
                  corr2 = unlist(corr2))


df2 %>% 
  ggplot() + 
  geom_density(aes(x=corr1))



df %>% 
  ggplot() + 
  geom_line(aes(x=t, y=cs1), col="red") +
  geom_line(aes(x=t, y=cs2), col="blue")



model <- lm(data = df, 
            formula = cs1 ~ cs2)

library(broom)

model2 <- augment_columns(model, data = df)


model2 %>%
  ggplot() +
  geom_point(aes(x = t, y =.resid))


acf(df$cs1)


pacf(df$cs2)











df %>% 
  ggplot() + 
  geom_line(aes(x=t, y=r1), col="red") +
  geom_line(aes(x=t, y=r2), col="blue")
