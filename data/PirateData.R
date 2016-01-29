# Create pirate dataset for YaRrr

# pirates
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
pirates$tchests <- round(rexp(n, 5 / (pirates$age + pirates$tattoos)), 0)

# Create parrots as a function of age
pirates$parrots <- round(rexp(n, 1 / pirates$age * 10), 0)

# Create favorate pirate as a function of sex

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

pirates$sword.time <- unlist(lapply(1:nrow(pirates), function(x) {

  sword.i <- pirates$sword.type[x]
  headband.i <- pirates$headband[x]

  sword.num.convert <- data.frame("sword" = c("cutlass", "sabre", "scimitar", "banana"),
                                  "num" = c(15, 2, 1, .0001)
  )

  headband.num.convert <- data.frame("headband" = c("yes", "no"),
                                     "num" = c(1, 5)
  )

  sword.num <- sword.num.convert$num[sword.num.convert$sword == sword.i]
  headband.num <- headband.num.convert$num[headband.num.convert$headband == headband.i]

  speed.i <- rexp(1, rate = (sword.num + headband.num / 5) / 10)


  return(round(speed.i, 2))

}))


# Create eye-patch as a function of age and parrots

pirates$eyepatch <- unlist(lapply(1:nrow(pirates), function(x) {

  age.i <- pirates$age[x]
  parrots.i <- pirates$parrots[x]

  patch.prob <- 1 / (1 + exp(-age.i / 50 - parrots.i / 20))

  patch.i <- sample(c(1, 0), size = 1, replace = T, prob = c(patch.prob, 1 - patch.prob))


  return(patch.i)

}))



# Create beard-length as a function of sex and tattoos

pirates$beard.length <- unlist(lapply(1:nrow(pirates), function(x) {

  sex.i <- pirates$sex[x]
  tattoos.i <- pirates$tattoos[x]

  if(sex.i == "male") {sex.num <- 10}
  if(sex.i == "female") {sex.num <- 0}
  if(sex.i == "other") {sex.num <- 5}

  beard.length <- rnorm(1, mean = sex.num + tattoos.i, sd = 5)

if(sex.i == "female") {beard.length <- rnorm(1, mean = 0, sd = 1)}
  if(beard.length < 0) {beard.length <- 0}

  return(round(beard.length, 0))

}))

# Create favorite pixar movie as a function of eyepatch

pirates$fav.pixar <- unlist(lapply(1:nrow(pirates), function(x) {

  movie.vec <- c("Toy Story",
                 "A Bug's Life",
                 "Toy Story 2",
                 "Monsters, Inc.",
                 "Finding Nemo",
                 "The Incredibles",
                 "Cars",
                 "Ratatouille",
                 "WALL-E",
                 "Up",
                 "Toy Story 3",
                 "Cars 2",
                 "Brave",
                 "Monsters University",
                 "Inside Out")

  patch.i <- pirates$eyepatch[x]
  prob.vec.1 <- c(10, 10, 10, 20, 200, 30, 10, 1, 25, 40, 5, 5, 10, 35, 5)
  prob.vec.2 <- c(10, 10, 10, 20, 10, 30, 10, 1, 25, 40, 5, 5, 10, 35, 200)

  if(patch.i == 0) {prob.vec.i <- prob.vec.1}
  if(patch.i > 0) {prob.vec.i <- prob.vec.2}

fav.pix <- sample(movie.vec, size = 1, prob = prob.vec.i / sum(prob.vec.i))


  return(fav.pix)

}))



# ------------------
# Write table to file
write.table(pirates, file = "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/data/pirates.txt", sep = "\t")
save(pirates, file = "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/yarrr/data/pirates.RData")
# ------------------

# ---------
# Create pirate.errors
# ----------


pirateserrors <- pirates

## Add some bad data
pirateserrors$sex[sample(1:nrow(pirateserrors), size = 3)] <- sample(c("yes please!", "sure I'll have some", "depends on who is offering"), size = 3, replace = F)
pirateserrors$age[sample(1:nrow(pirateserrors), size = 20)] <- sample(c(999, 0, -99, 500, 12345), size = 20, replace = T)
pirateserrors$headband[sample(1:nrow(pirateserrors), size = 10)] <- sample(c("sometimes", "what is a headband?"), size = 10, replace = T)
pirateserrors$college[sample(1:nrow(pirateserrors), size = 10)] <- sample(c(NA), size = 10, replace = T)
pirateserrors$tattoos[sample(1:nrow(pirateserrors), size = 10)] <- sample(c(1000000, -10, NA), size = 10, replace = T)
pirateserrors$favorite.pirate[sample(1:nrow(pirateserrors), size = 10)] <- sample(c("your mom"), size = 10, replace = T)
pirateserrors$eyepatch[sample(1:nrow(pirateserrors), size = 20)] <- sample(c(2:10), size = 20, replace = T)
pirateserrors$tchests[sample(1:nrow(pirateserrors), size = 20)] <- sample(1000:10000, size = 20, replace = T)
pirateserrors$parrots[sample(1:nrow(pirateserrors), size = 20)] <- sample(500:1000, size = 20, replace = T)


# Add NAs in random locations

row.vec <- sample(1:nrow(pirateserrors), size = 50, replace = T)
col.vec <- sample(1:ncol(pirateserrors), size = 50, replace = T)

for(i in 1:length(row.vec)) {

pirateserrors[row.vec[i], col.vec[i]] <- NA

}


# Write table to file
write.table(pirateserrors, file = "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/data/pirateserrors.txt", sep = "\t")
save(pirateserrors, file = "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/yarrr/data/pirateserrors.RData")

}


# auction
{
auction <- data.frame(cannons = sample(c(seq(2, 20, 2)), size = 1000, replace = T, prob = 10:1 / sum(10:1)),
                    rooms = sample(seq(1, 60, 1), size = 1000, replace = T, prob = c(1:30, 30:1) / sum(c(1:30, 30:1))),
                    age = round(rnorm(1000, mean = 50, sd = 20), 1),
                    condition = sample(10:1, size = 1000, replace = T, prob = c(1:5, 5:1)),
                    color = sample(c("black", "brown", "red", "salmon", "plum"), size = 1000, prob = c(.5, .3, .1, .05, .05), replace = T),
                    stringsAsFactors = F
)

auction$age[auction$age < 0] <- 0

auction$weight <- round(with(auction,
                       cannons * 500 + rooms * 100 + rnorm(nrow(auction), mean = 1000, sd = 100)), 0)

auction$style <- sapply(1:nrow(auction), function(x) {
  sample(c("modern", "classic"), 1,
         prob = c(1 - 1 / (1 + exp(-((auction$age[x] - 50) / 10))),
                  1 / (1 + exp(-((auction$age[x] - 50) / 10)))))})

auction$price <- round(with(auction,
                     -2000 +
                     100 * cannons +
                     50 * rooms +
                     (-10) * age +
                     100 * condition + rnorm(nrow(auction), mean = 0, sd = 200)
), 0)


auction$price[auction$color == "black"] <- auction$price[auction$color == "black"] + 100
auction$price[auction$color == "brown"] <- auction$price[auction$color == "brown"] + 0
auction$price[auction$color == "red"] <- auction$price[auction$color == "red"] - 500
auction$price[auction$color == "salmon"] <- auction$price[auction$color == "salmon"] + 200
auction$price[auction$color == "plum"] <- auction$price[auction$color == "plum"] + 200


auction$price[auction$price < 0] <- 0
auction$sold <- as.numeric(auction$price > 0)



auction$jbb <- round(with(auction,
                            1000 +
                              100 * cannons +
                              50 * rooms +
                              (-50) * age +
                              20 * condition
), 0)


auction$jbb[auction$color == "black"] <- auction$jbb[auction$color == "black"] + 100
auction$jbb[auction$color == "brown"] <- auction$jbb[auction$color == "brown"] + 0
auction$jbb[auction$color == "red"] <- auction$jbb[auction$color == "red"] - 500
auction$jbb[auction$color == "salmon"] <- auction$jbb[auction$color == "salmon"] + 200
auction$jbb[auction$color == "plum"] <- auction$jbb[auction$color == "plum"] + 200


auction$jbb[auction$jbb < 0] <- 0

auction$jbb <- round(auction$jbb / 1000, 1) * 1000


 head(auction)
summary(lm(price~., data = auction))
#anova(lm(price~., data = auction))

save(auction, file = "~/Dropbox/Git/YaRrr_Book/yarrr/data/auction.RData")

write.table(auction, "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/data/auction.txt", sep = "\t")

}


# capture
{
set.seed(100)
 # ship size
capture <- data.frame(
  size = round(rnorm(1000, mean = 50, sd = 4), 0)
)

# number of cannons

capture <- capture %>%
  mutate(
    cannons = round(log(size * 10) + rnorm(1000, mean = 10, sd = 10), 0) * 2,
    cannons = ifelse(cannons < 0, 0, cannons)
  )

# style

capture <- capture %>%
  mutate(
    style = sample(c("classic", "modern"), size = 1000, prob = c(.1, .9), replace = T),
    warnshot = sample(c(1, 0), size = 1000, prob = c(.2, .8), replace = T),
    date = sample(1:365, size = 1000, replace = T),
    heardof = sample(c(1, 0), size = 1000, prob = c(.3, .7), replace = T),
    decorations = sample(1:10, size = 1000, prob = 10:1, replace = T),
    daysfromshore = sample(1:30, size = 1000, replace = T)
  )

head(capture)

capture <- capture %>%
  mutate(
    style.temp = ifelse(style == "modern", 5, 0),
    warnshot.temp = ifelse(warnshot == 1, 100, 0),
    heardof.temp = ifelse(heardof == 1, 15, 0)  )

capture$decorations.temp <- capture$decorations
capture$decorations.temp[capture$decorations == 1] <- sample(c(0, 100), size = sum(capture$decorations == 1), replace = T, prob = c(.5, .5))

capture$speed <- round(rnorm(1000, mean = 20, sd = 3), 0)



# Create gold found

capture$treasure <- round(with(capture,
                         size * 20 +
                           cannons * 20 +
                           style.temp +
                           warnshot.temp +
                           date * 0 +
                           heardof.temp +
                           decorations.temp * 30 +
                           daysfromshore * -10 +
                           speed * 10,
                           rnorm(1000, mean = 100, sd = 5), 0)
                         )

capture <- capture[,grepl(".temp", names(capture)) == F]

summary(lm(treasure ~., data = capture))

write.table(capture, "/Users/Nathaniel/Dropbox/Git/YaRrr_Book/data/capture.txt", sep = "\t")

save(capture, file = "~/Dropbox/Git/YaRrr_Book/yarrr/data/capture.RData")
}

# BeardLengths
{
BeardLengths <- data.frame(Ship = rep(c("Angry Badger",
                                        "Fearless Snake",
                                        "Nervous Goat"), each = 50),
                           Beard = c(rnorm(50, mean = 22, sd = 5),
                                     rnorm(50, mean = 19, sd = 3),
                                     rnorm(25, mean = 40, sd = 3),
                                     round(rexp(n = 25, rate = .9))
                           )
)


BeardLengths$Beard <- round(BeardLengths$Beard, 1)
rownames(BeardLengths) <- 1:nrow(BeardLengths)
BeardLengths <- BeardLengths[sample(nrow(BeardLengths),
                                    size = nrow(BeardLengths)),]

save(BeardLengths, file = "data/BeardLengths.RData")
}
