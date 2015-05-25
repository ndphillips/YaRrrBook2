



n.samp <- 100

hsurvey <- data.frame("sex" = sample(c("male", "female", "other/NA"), size = n.samp, replace = T, prob = c(.45, .45, .1)),
                     "age" = sample(18:60, size = n.samp, replace = T),
                     "income" = sample(seq(25000, 100000, 5000), size = n.samp, replace = T),
                     "country" = sample(c("USA", "Canada", "Mexico", "India", "Portugal"), size = n.samp, replace = T),
                     "happiness" = sample(8, size = n.samp, replace = T),
                     "siblings" = sample(0:3, size = n.samp, replace = T),
                     stringsAsFactors = F
                     )

hsurvey$siblings[sample(n.samp, size = 10)] <- sample(c(-1, 100, -10), size = 10, replace = T)
hsurvey$income[sample(n.samp, size = 15)] <- sample(c(-1000000, -1, 10000000, 999999999, -5), size = 15, replace = T)
hsurvey$age[sample(n.samp, size = 5)] <- sample(c(0, -10, 283, 999), size = 5, replace = T)
hsurvey$happiness[sample(n.samp, size = 10)] <- sample(c(11, 0), size = 5, replace = T)


write.table(hsurvey, file = "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/data/hsurvey.txt", sep = "\t")




n.samp <- 100

planks <- data.frame("plank.type" = sample(c("cricket", "james", "leitz"), size = n.samp, replace = T),
                     ""
                     stringsAsFactors = F
)

survey$siblings[sample(n.samp, size = 10)] <- sample(c(-1, 100, -10), size = 10, replace = T)
survey$income[sample(n.samp, size = 15)] <- sample(c(-1000000, -1, 10000000, 999999999, -5), size = 15, replace = T)
survey$age[sample(n.samp, size = 5)] <- sample(c(0, -10, 283, 999), size = 5, replace = T)
survey$happiness[sample(n.samp, size = 10)] <- sample(c(11, 0), size = 5, replace = T)


write.table(survey, file = "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/data/happiness.txt", sep = "\t")
