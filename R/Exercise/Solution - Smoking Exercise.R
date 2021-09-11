library(tidyverse)
library(readxl)

smoke <- read_excel(file.choose())

head(smoke)
summary(smoke)

hist(smoke$slst30a1)

smoke_small <- smoke$slst30a1[smoke$slst30a1 < 95]

#or using dplyr

other_smoke_small <- filter(smoke, smoke$slst30a1 < 95)


hist(other_smoke_small$slst30a1)

hist(smoke_small)

smoke_smallv2 <- ifelse(smoke_small == 1, 0,
                        ifelse( smoke_small == 2, 1,
                                ifelse(smoke_small == 3, 2.5,
                                        ifelse(smoke_small == 4, 4.5,
                                               ifelse(smoke_small == 5, 8.5,
                                                      ifelse(smoke_small == 6, 15.5,
                                                             ifelse(smoke_small == 7, 25.5,
                                                                    ifelse(smoke_small == 8, 30, 0))))))))

smoke_smallv3 <- recode(smoke_small, '1'=0, '2'=1, '3'=2.5, '4'=4.5, '5'=8.5, '6' = 15.5, '7' = 25.5, '8'=30)

hist(smoke_smallv2)
hist(smoke_smallv3)


white_noise <- c(1.1245341, -0.1190138, -0.1215794)
filter(white_noise, sides = 2, filter = rep(1/3, 3))

x <- ts(seq(100, 300, by = 100), start = 2)
y <- ts(1:3)
x + y


t <- ts(1:5, start = 2)
lagged_t <- lag(t, k = -1)
time(lagged_t)
