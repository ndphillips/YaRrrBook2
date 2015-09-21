# Create pirate dataset for YaRrr
{
n <- 1000

pirates <- data.frame("id" = 1:n,
                      "sex" = sample(c("male", "female", "other"), size = n, replace = T, prob = c(.48, .48, .04)),
                      "headband" = sample(c("yes", "no"), size = n, replace = T, prob = c(.9, .1)),
                      "age" = round(rnorm(n, 30, 7), 0), stringsAsFactors = F
)

n.males <- sum(pirates$sex == "male")
n.females <- sum(pirates$sex == "female")
n.other <- sum(pirates$sex == "other")


# Create age as a function of sex

pirates$age[pirates$sex == "male"] <- round(rnorm(n.males, mean = 25, sd = 5))
pirates$age[pirates$sex == "female"] <- round(rnorm(n.females, mean = 30, sd = 5))
pirates$age[pirates$sex == "other"] <- round(rnorm(n.other, mean = 27.5, sd = 5))

# Create college as a function of age

college.p <- 1 / (1 + exp(-pirates$age + 30))

pirates$college <- unlist(lapply(1:n, function(x) {sample(c("JSSFP", 
                                                            "CCCC"), size = 1, 
                                                          prob = c(college.p[x], 1 - college.p[x]))}))


# Create tattoos as a function of headband use

pirates$tattoos[pirates$headband == "yes"] <- round(rnorm(sum(pirates$headband == "yes"), mean = 10, sd = 3), 0)
pirates$tattoos[pirates$headband == "no"] <- round(rnorm(sum(pirates$headband == "no"), mean = 5, sd = 3), 0)
pirates$tattoos[pirates$tattoos < 0] <- 0


# Create tchests found as a function of age and tattoos
pirates$tchests.found <- round(rexp(n, 5 / (pirates$age + pirates$tattoos)), 0)

# Create parrots.lifetime as a function of age
pirates$parrots.lifetime <- round(rexp(n, 1 / pirates$age * 10), 0)

# Create favoriate pirate as a function of sex

pirates$favorite.pirate[pirates$sex == "male"] <- sample(x = c("Jack Sparrow", "Blackbeard", "Lewis Scot", "Hook", "Edward Low", "Anicetus"),
                                                         size = sum(pirates$sex == "male"),
                                                         replace = T,
                                                         prob = c(.75, .05, .05, .05, .05, .05)
)

pirates$favorite.pirate[pirates$sex != "male"] <- sample(x = c("Jack Sparrow", "Blackbeard", "Lewis Scot", "Hook", "Edward Low", "Anicetus"),
                                                         size = sum(pirates$sex != "male"),
                                                         replace = T,
                                                         prob = rep(1/6, times = 6)
)


# Create sword type as a function of headband

pirates$sword.type[pirates$headband == "yes"] <- sample(c("cutlass", "sabre", "scimitar", "banana"), 
                                                        size = sum(pirates$headband == "yes"), replace = T, 
                                                        prob = c(.9, .04, .04, .01))

pirates$sword.type[pirates$headband == "no"] <- sample(c("cutlass", "sabre", "scimitar", "banana"), 
                                                       size = sum(pirates$headband == "no"), replace = T, 
                                                       prob = c(.1, .3, .3, .3))

# Create sword speed as a function of bandana use and sword.type

pirates$sword.speed <- unlist(lapply(1:nrow(pirates), function(x) {
  
  sword.i <- pirates$sword.type[x]
  headband.i <- pirates$headband[x]
  
  sword.num.convert <- data.frame("sword" = c("cutlass", "sabre", "scimitar", "banana"),
                                  "num" = c(15, 2, 1, .001)
  )
  
  headband.num.convert <- data.frame("headband" = c("yes", "no"),
                                     "num" = c(1, 5)
  )
  
  sword.num <- sword.num.convert$num[sword.num.convert$sword == sword.i]
  headband.num <- headband.num.convert$num[headband.num.convert$headband == headband.i] 
  
  speed.i <- rexp(1, rate = (sword.num + headband.num) / 10)
  
  speed.i
  
  return(speed.i)
  
}))


# Write table to file
write.table(pirates, file = "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/data/pirate_survey_noerrors.txt", sep = "\t")
save(pirates, file = "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/yarrr/data/pirates.RData")

pirates.errors <- pirates

## Add some bad data
pirates.errors$sex[sample(1:nrow(pirates.errors), size = 3)] <- sample(c("yes please!", "sure I'll have some", "depends on who is offering"), size = 3, replace = F)
pirates.errors$age[sample(1:nrow(pirates.errors), size = 20)] <- sample(c(999, 0, -99, 500, 12345), size = 20, replace = T)
pirates.errors$headband[sample(1:nrow(pirates.errors), size = 10)] <- sample(c("sometimes", "what is a headband?"), size = 10, replace = T)
pirates.errors$college[sample(1:nrow(pirates.errors), size = 10)] <- sample(c(NA), size = 10, replace = T)
pirates.errors$tattoos[sample(1:nrow(pirates.errors), size = 10)] <- sample(c(1000000, -10, NA), size = 10, replace = T)
pirates.errors$favorite.pirate[sample(1:nrow(pirates.errors), size = 10)] <- sample(c("your mom"), size = 10, replace = T)

# Write table to file
write.table(pirates.errors, file = "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/data/pirate_survey_witherrors.txt", sep = "\t")

}


# Ship dataset

shipauction <- data.frame(cannons = sample(c(seq(2, 20, 2)), size = 1000, replace = T),
                    rooms = sample(seq(10, 60, 4), size = 1000, replace = T),
                    age = round(rnorm(1000, mean = 50, sd = 10), 1),
                    style = sample(c("modern", "classic"), size = 1000, replace = T),
                    condition = sample(10:1, size = 1000, replace = T, prob = c(1:5, 5:1)),
                    weight = rnorm(1000, 5000, 500),
                    color = sample(c("black", "brown", "red"), size = 1000, prob = c(.5, .3, .2), replace = T),
                    stringsAsFactors = F
)




shipauction$style <- sapply(1:nrow(shipauction), function(x) {
  sample(c("modern", "classic"), 1, 
         prob = c(1 - 1 / (1 + exp(-((shipauction$age[x] - 50) / 10))), 
                  1 / (1 + exp(-((shipauction$age[x] - 50) / 10)))))})

shipauction$price[shipauction$style == "modern"] <- with(shipauction[shipauction$style == "modern",], 
                                       10000 + 
                                       100 * cannons + 
                                       500 * rooms + 
                                       (-500) * age + 
                                       200 * condition
)


shipauction$price[shipauction$style == "classic"] <- with(shipauction[shipauction$style == "classic",],
                                                          0 + 
                                                           100 * cannons + 
                                                           500 * rooms + 
                                                           (300) * age + 
                                                           200 * condition
)

shipauction$price[shipauction$color == "black"] <- shipauction$price[shipauction$color == "black"] + 10000
shipauction$price[shipauction$color == "black"] <- shipauction$price[shipauction$color == "brown"] + 0
shipauction$price[shipauction$color == "black"] <- shipauction$price[shipauction$color == "red"] -5000

shipauction$price <- round(shipauction$price + rnorm(1000, mean = 0, sd = 4000), 0)

summary(lm(price~., data = shipauction))

write.table(shipauction, "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/data/shipauction.txt", sep = "\t")
