#       setwd("~/Google Drive File Stream/My Drive/Scout/User Studies/Voice Comparison")
require(ggplot2)
data = read.csv('/Users/jmaddock/data/voice_comparison/simplified-data_old.csv')

if(!require(EMT)){install.packages("EMT")}
if(!require(rcompanion)){install.packages("rcompanion")}

data = as.data.frame(lapply(data, function (x) if (is.factor(x)) factor(x) else x))

data$gender = factor(data$gender, levels = c("", "Man" , "These don't describe me",   "Woman"),
              labels = c("NA", "Male", "Other", "Female"))

data$check1 = ifelse(data$check1 == "An article about dealing with stress", 1, 0)
data$check2 = ifelse(data$check2 == "His constant need for success", 1, 0)
data$check3 = ifelse(data$check3 == "It oxygenates your brain", 1, 0)
data$check4 = ifelse(data$check4 == "2 and 3", 1, 0)
data$check5 = ifelse(data$check5 == "Ask yourself if a thought is really true.", 1, 0)
data$check6 = ifelse(data$check6 == "Everything has to have a purpose", 1, 0)
data$check7 = ifelse(data$check7 == "Do less than you want to do", 1, 0)

#data$grade = round(rowMeans(data[, 15:21], na.rm = TRUE),2)*10

data$rate.speed = ordered(data$rate.speed, levels = c("Much too fast", "Too fast", "Just right",
                                                      "Too slow", "Much too slow"))

#write.csv(data, file = "simplified-data.csv")

# Distributions ----
summary(data)

## Demographics ----
table(data$gender)
prop.table(table(data$gender, data$voice), margin=2)*100

require(psych)
describeBy(data$age)
describeBy(data$age, group = data$voice)

#table(data$income)
#table(data$state)
table(data$device)
table(data$headphones)

## Audio paused ----
#table(data$voice, data$audio.paused)
#table(data$voice, data$audio.paused, data$gender)

summary(data$audio.paused[data$voice == "Google A"])
summary(data$audio.paused[data$voice == "Google C"])
summary(data$audio.paused[data$voice == "Polly Sally"])
summary(data$audio.paused[data$voice == "Polly Joana"])
summary(data$audio.paused[data$voice == "Polly Matthew"])
summary(data$audio.paused[data$voice == "Voicery Nichole"])
summary(data$audio.paused[data$voice == "Deep Speech"])
summary(data$audio.paused[data$voice == "Android UK Male"])
summary(data$audio.paused[data$voice == "iOS Female"])
summary(data$audio.paused[data$voice == "Deep Speech Nancy"])
summary(data$audio.paused[data$voice == ".Jofish"])
summary(data$audio.paused[data$voice == ".Janice"])
summary(data$audio.paused[data$voice == ".Abe"])
summary(data$audio.paused[data$voice == "Windows Male"])
summary(data$audio.paused[data$voice == "Windows Female"])
summary(data$audio.paused[data$voice == "Mac Default"])

# where did people stop listening to it?
group_names.voice = c(
  "Deep Speech" = paste("Deep Speech (", nrow(data[data$voice == "Deep Speech",]) ,")"),
  "Google A" = paste("Google A (", nrow(data[data$voice == "Google A",]) ,")"),
  "Google C" = paste("Google C (", nrow(data[data$voice == "Google C",]) ,")"),
  "Polly Joana" = paste("Polly Joana (", nrow(data[data$voice == "Polly Joana",]) ,")"),
  "Polly Matthew" = paste("Polly Matthew (", nrow(data[data$voice == "Polly Matthew",]) ,")"),
  "Polly Sally" = paste("Polly Sally (", nrow(data[data$voice == "Polly Sally",]) ,")"),
  "Voicery Nichole" = paste("Voicery Nichole (", nrow(data[data$voice == "Voicery Nichole",]) ,")"),
  "Android UK Male" = paste("Android UK Male (", nrow(data[data$voice == "Android UK Male",]) ,")"),
  "iOS Female" = paste("iOS Female (", nrow(data[data$voice == "iOS Female",]) ,")"),
  "Deep Speech Nancy" = paste("Deep Speech Nancy (", nrow(data[data$voice == "Deep Speech Nancy",]) ,")"),
  ".Jofish" = paste(".Jofish (", nrow(data[data$voice == ".Jofish",]) ,")"),
  ".Janice" = paste(".Janice (", nrow(data[data$voice == ".Janice",]) ,")"),
  ".Abe" = paste(".Abe (", nrow(data[data$voice == ".Abe",]) ,")")
)

ggplot(subset(data, audio.paused < 280), aes(x = audio.paused, color = voice, fill = voice)) +
  geom_histogram(stat = "bin", binwidth = 25) +
  #geom_density(alpha = 0.1) +
  facet_wrap( ~ voice, ncol = 4, labeller = as_labeller(group_names.voice), drop = TRUE) +
  guides(color = FALSE, fill=FALSE) + 
#  xlab("When did people stop listening (before the end)") +
#  ylab("Count") +
  theme_classic() + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"),
        text = element_text(size=25),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(3, "lines"))

## Rating experience ----
prop.table(table(data$voice, data$rate.experience), margin = 1)*100

# broken down by gender ----
group_names.gender = c(
  "Male" = paste("Male (", nrow(data[data$gender == "Male",]) ,")"),
  "Female" = paste("Female (", nrow(data[data$gender == "Female",]) ,")")
)



#Rating chart by gender
temp = as.data.frame(round(prop.table(table(data$voice, data$rate.experience, data$gender), c(3,1))[,,-c(1,3)], digits = 3)*100)
ggplot(temp, aes(x = Var1, y = Freq, fill = Var2, 
                 label = Freq)) +
  geom_bar(stat="identity") + 
  facet_wrap( ~ Var3, labeller = as_labeller(group_names.gender)) +
  theme_classic() + 
  theme(plot.margin=unit(c(.5,.5,.5,1.5),"cm"),
        text = element_text(size=20),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(3, "lines"),
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "Rating")) +
  scale_fill_brewer(palette = "BrBG", direction = -1)

### Fisher’s exact test of independence
ratingsmatrix <-data.matrix(temp)

summary(temp)


  
fisher.test(ratingsmatrix,
            alternative="two.sided")

#Rating speed chart
temp = as.data.frame(round(prop.table(table(data$voice, data$rate.experience), c(3,1))[,,-c(1,3)], digits = 3)*100)
ggplot(temp, aes(x = Var1, y = Freq, fill = Var2, 
                 label = Freq)) +
  geom_bar(stat="identity") + 
  #  facet_wrap( ~ Var3, labeller = as_labeller(group_names.gender)) +
  theme_classic() + 
  theme(plot.margin=unit(c(.5,.5,.5,1.5),"cm"),
        text = element_text(size=20),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(3, "lines"),
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "Rating")) +
  scale_fill_brewer(palette = "BrBG", direction = -1)

# broken down by headphone use ----
group_names.headphone = c(
  "No" = paste("No headphones (", nrow(data[data$headphones == "No",]) ,")"),
  "Yes" = paste("Headphones (", nrow(data[data$headphones == "Yes",]) ,")")
)

temp = as.data.frame(round(prop.table(table(data$voice, data$rate.experience, data$headphones), c(3,1)), digits = 3)*100)
ggplot(temp, aes(x = Var1, y = Freq, fill = Var2, 
                 label = Freq)) +
  geom_bar(stat="identity") + 
  facet_wrap( ~ Var3, labeller = as_labeller(group_names.headphone)) +
  theme_classic() + 
  theme(plot.margin=unit(c(.5,.5,.5,1.5),"cm"),
        text = element_text(size=20),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(3, "lines"),
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "Rating")) +
  scale_fill_brewer(palette = "BrBG", direction = -1)

## Rate speed ----
temp = as.data.frame(round(prop.table(table(data$voice, data$rate.speed), margin = 1), digits = 3)*100)
ggplot(temp, aes(x = Var1, y = Freq, fill = Var2, 
                 label = Freq)) +
  geom_bar(stat="identity") + 
  theme_classic() + 
  theme(plot.margin=unit(c(.5,.5,.5,1.5),"cm"),
        text = element_text(size=20),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(3, "lines"),
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "Rating")) +
  scale_fill_brewer(palette = "BrBG", direction = -1)

# speed rating broken down by gender ----
temp = as.data.frame(round(prop.table(table(data$voice, data$rate.speed, data$gender), c(3,1))[,,-c(1,3)], digits = 3)*100)
ggplot(temp, aes(x = Var1, y = Freq, fill = Var2, 
                 label = Freq)) +
  geom_bar(stat="identity") + 
  facet_wrap( ~ Var3, labeller = as_labeller(group_names.gender)) +
  theme_classic() + 
  theme(plot.margin=unit(c(.5,.5,.5,1.5),"cm"),
        text = element_text(size=20),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(3, "lines"),
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "Rating")) +
  scale_fill_brewer(palette = "BrBG", direction = -1)

## Re-listen rating ----
table(data$voice, data$rate.again)

summary(data$rate.again[data$voice == "Google A"])
summary(data$rate.again[data$voice == "Google C"])
summary(data$rate.again[data$voice == "Polly Sally"])
summary(data$rate.again[data$voice == "Polly Joana"])
summary(data$rate.again[data$voice == "Polly Matthew"])
summary(data$rate.again[data$voice == "Voicery Nichole"])
summary(data$rate.again[data$voice == "Deep Speech"])

median = aggregate(rate.again ~ voice, data=data, FUN = "median")

ggplot(data, aes(x = rate.again, color = voice, fill = voice)) +
  #geom_histogram(stat = "bin", binwidth = 1) +
  geom_density(alpha = 0.1) +
  facet_wrap( ~ voice, ncol = 4, labeller = as_labeller(group_names.voice), drop = TRUE) +
  guides(color = FALSE, fill=FALSE) + 
  theme_classic() + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"),
        text = element_text(size=25),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(3, "lines")) +
  geom_vline(data=median, aes(xintercept=rate.again, color=voice),
             linetype="dashed")

# broken down by gender ----
median.gender = aggregate(rate.again ~ voice + gender, 
                          data=subset(data, gender %in% c("Male", "Female")), FUN = "median")

ggplot(subset(data, gender %in% c("Male", "Female")), aes(x = rate.again, color = gender, fill = gender)) +
  geom_density(alpha = 0.1) +
  facet_wrap( ~ voice, ncol = 7, labeller = as_labeller(group_names.voice), drop = TRUE) +
  theme_classic() + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"),
        text = element_text(size=15),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(3, "lines"),
        legend.position = "bottom") +
  geom_vline(data=median.gender, aes(xintercept=rate.again, color=gender),
             linetype="dashed")

# broken down by headphone use ----
median.headphones = aggregate(rate.again ~ voice + headphones, data=data, FUN = "median")

ggplot(data, aes(x = rate.again, color = headphones, fill = headphones)) +
  geom_density(alpha = 0.1) +
  facet_wrap( ~ voice, ncol = 7, labeller = as_labeller(group_names.voice), drop = TRUE) +
  theme_classic() + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"),
        text = element_text(size=15),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(3, "lines"),
        legend.position = "bottom") +
  geom_vline(data=median.headphones, aes(xintercept=rate.again, color=headphones),
             linetype="dashed")


## Voice opinions ----
round(table(data$voice, data$voice.easy)/(as.vector(t(table(data$voice)))), digits = 3)*100
round(table(data$voice, data$voice.monotone)/(as.vector(t(table(data$voice)))), digits = 3)*100
round(table(data$voice, data$voice.natural)/(as.vector(t(table(data$voice)))), digits = 3)*100
round(table(data$voice, data$voice.no_emotion)/(as.vector(t(table(data$voice)))), digits = 3)*100
round(table(data$voice, data$voice.focus)/(as.vector(t(table(data$voice)))), digits = 3)*100

## Comprehension ----
summary(table(data$voice, data$grade))
plot(jitter(data$grade[data$voice == "Google A"]), jitter(data$rate.again[data$voice == "Google A"]))
plot(jitter(data$grade[data$voice == "Google C"]), jitter(data$rate.again[data$voice == "Google C"]))
plot(jitter(data$grade[data$voice == "Polly Sally"]), jitter(data$rate.again[data$voice == "Polly Sally"]))
plot(jitter(data$grade[data$voice == "Polly Joana"]), jitter(data$rate.again[data$voice == "Polly Joana"]))
plot(jitter(data$grade[data$voice == "Polly Matthew"]), jitter(data$rate.again[data$voice == "Polly Matthew"]))
plot(jitter(data$grade[data$voice == "Voicery Nichole"]), jitter(data$rate.again[data$voice == "Voicery Nichole"]))
plot(jitter(data$grade[data$voice == "Deep Speech"]), jitter(data$rate.again[data$voice == "Deep Speech"]))
plot(jitter(data$grade[data$voice == "Android UK Male"]), jitter(data$rate.again[data$voice == "Android UK Male"]))
plot(jitter(data$grade[data$voice == "iOS Female"]), jitter(data$rate.again[data$voice == "iOS Female"]))
plot(jitter(data$grade[data$voice == "Deep Speech Nancy"]), jitter(data$rate.again[data$voice == "Deep Speech Nancy"]))
plot(jitter(data$grade[data$voice == "Text only"]), jitter(data$rate.again[data$voice == "Text Only "]))

median.grade = aggregate(grade ~ voice, data=data, FUN = "median")
mean.grade = aggregate(grade ~ voice, data=data, FUN = "mean")


ggplot(data, aes(x = grade, color = voice, fill = voice)) +
  geom_density(alpha = 0.1) +
  facet_wrap( ~ voice, ncol = 4, labeller = as_labeller(group_names.voice), drop = TRUE) +
  guides(color = FALSE, fill=FALSE) + 
  theme_classic() + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"),
        text = element_text(size=25),
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(3, "lines")) +
  geom_vline(data=median.grade, aes(xintercept=grade, color=voice),
             linetype="dashed")



# broken down by gender ----
ggplot(subset(data, gender %in% c("Male", "Female")), aes(x = grade, color = voice, fill = voice)) +
  geom_density(alpha = 0.1) +
  facet_wrap( ~ gender, labeller = as_labeller(group_names.gender), drop = TRUE)

