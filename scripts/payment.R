setwd("~/Google Drive File Stream/My Drive/Scout/User Studies/Voice Comparison/data")
require(ggplot2)

# Read in survey gizmo data, SG case minus 1----
v0 = read.csv("voice0.csv", na.strings=c("",NA))
v1 = read.csv("voice1.csv", na.strings=c("",NA))
v2 = read.csv("voice2.csv", na.strings=c("",NA))
v3 = read.csv("voice3.csv", na.strings=c("",NA))
v4 = read.csv("voice4.csv", na.strings=c("",NA))
v5 = read.csv("voice5.csv", na.strings=c("",NA))
v6 = read.csv("voice6.csv", na.strings=c("",NA))
v7 = read.csv("voice7.csv", na.strings=c("",NA))
v8 = read.csv("voice8.csv", na.strings=c("",NA))
v9 = read.csv("voice9.csv", na.strings=c("",NA))
v10 = read.csv("voice10.csv", na.strings=c("",NA))
v11 = read.csv("voice11.csv", na.strings=c("",NA))
v12 = read.csv("voice12.csv", na.strings=c("",NA))
v13 = read.csv("voice13.csv", na.strings=c("",NA))
v14 = read.csv("voice14.csv", na.strings=c("",NA))
v15 = read.csv("voice15.csv", na.strings=c("",NA))
v16 = read.csv("voice16.csv", na.strings=c("",NA))
v17 = read.csv("voice17.csv", na.strings=c("",NA))

# Clean and assign voice ----
v0 = v0[,-c(22,23,24,25,27,28,29)]
v0$Voice = "Google A"
v1$Voice = "Google C"
v2$Voice = "Polly Sally"
v3$Voice = "Polly Matthew"
v4$Voice = "Voicery Nichole"
v5$Voice = "MozillaTTS:LJSpeech"
v6$Voice = "Polly Joana"
v7$Voice = "Android UK Male"
v8$Voice = "iOS Female"
v9$Voice = "Mozilla TTS:Nancy"
v10$Voice = "Text only"
v11$Voice = "Mozilla TTS:Nancy2"
v12$Voice = ".Jofish"
v13$Voice = ".Abe"
v14$Voice = ".Janice"
v15$Voice = "Windows Male"
v16$Voice = "Windows Female"
v17$Voice = "Mac Default"

# Read in MTurk data ----
b10 = read.csv("batch10.csv")
b40 = read.csv("batch40.csv")
b200 = read.csv("batch200.csv")
bjo = read.csv("batchJoana.csv")
bds = read.csv("batchDS.csv")
bjan2019 = read.csv("batchjan2019.csv")
bapril2019 = read.csv("batchapril2019.csv")
bapril2019j = read.csv("batchapril2019j.csv")
bos = read.csv("batchos.csv")

# Manual fixes for users ----
whitelist = c(
  'AX1THMSZH9Y8O', #mistyped code
  'A3NMQ3019X6YE0', # emailed code
  'A3E73BVSABJ7LM','AAQREZOK13OV7', 'A3IF13XHP5UPO2', #Complained
  'A3LL096CAY5WHB', 'A1QQQ7KKM896QY', 'A31681CCEVDIH3', 'A3LL096CAY5WHB' #emailed
)

# Merge Survey Gizmo data and remove incomplete ----
data = rbind(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17)
remove(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17)
data = data[ which(data$Status == "Complete"), ]

reject = data[which(data$Audio.last.paused.at. < 10 |
                      data$Time.on.page < 10 ),]
#write.csv(reject, file = "reject.csv")

# Payment check ----
b10$temp  = with(b10, interaction(Answer.Q2,  WorkerId))
b40$temp  = with(b40, interaction(Answer.Q2,  WorkerId))
b200$temp  = with(b200, interaction(Answer.Q2,  WorkerId))
bjo$temp  = with(bjo, interaction(Answer.Q2,  WorkerId))
bds$temp  = with(bds, interaction(Answer.Q2,  WorkerId))
bjan2019$temp  = with(bjan2019, interaction(Answer.Q2,  WorkerId))
bapril2019$temp  = with(bapril2019, interaction(Answer.Q2,  WorkerId))
bapril2019j$temp  = with(bapril2019j, interaction(Answer.Q2,  WorkerId))
bos$temp  = with(bos, interaction(Answer.Q2,  WorkerId))

data$temp = with(data, interaction(SessionID, URL.Variable..mid))
data$temp = with(data, interaction(SessionID, URL.Variable..mid))
reject$temp = with(reject, interaction(SessionID, URL.Variable..mid))
reject$temp = factor(reject$temp)

b10$Reject = ifelse(b10$temp %in% data$temp, 
                    ifelse(!(b10$temp %in% reject$temp), "",
                           "I'm sorry, but the data collected indicates you did not listen to the audio for sufficient time to complete the task properly"),
                    "I'm sorry, but we could not find your submission in our system.")
b10$Approve = ifelse(b10$temp %in% data$temp &
                       b10$Reject == "", "x", "")
b10$UPDATE.CompletedVoiceComparison = 1

b40$Reject = ifelse(b40$temp %in% data$temp, 
                    ifelse(!(b40$temp %in% reject$temp), "",
                           "I'm sorry, but the data collected indicates you did not listen to the audio for sufficient time to complete the task properly"),
                    "I'm sorry, but we could not find your submission in our system.")
b40$Approve = ifelse(b40$temp %in% data$temp &
                       b40$Reject == "", "x", "")
b40$UPDATE.CompletedVoiceComparison = 1

b200$Reject = ifelse(b200$WorkerId %in% whitelist, "", 
                     ifelse(b200$temp %in% data$temp, 
                            ifelse(!(b200$temp %in% reject$temp), "",
                                   "I'm sorry, but the data collected indicates you did not listen to the audio for sufficient time to complete the task properly"),
                            "I'm sorry, but we could not find a complete submission with your information in our system."))
b200$Approve = ifelse((b200$temp %in% data$temp & b200$Reject == "") |
                        b200$WorkerId %in% whitelist, "x", "")
b200$UPDATE.CompletedVoiceComparison = 1

bjo$Reject = ifelse(bjo$WorkerId %in% whitelist, "", 
                     ifelse(bjo$temp %in% data$temp, 
                            ifelse(!(bjo$temp %in% reject$temp), "",
                                   "I'm sorry, but the data collected indicates you did not listen to the audio for sufficient time to complete the task properly"),
                            "I'm sorry, but we could not find a complete submission with your information in our system."))
bjo$Approve = ifelse((bjo$temp %in% data$temp & bjo$Reject == "") |
                        bjo$WorkerId %in% whitelist, "x", "")
bjo$UPDATE.CompletedVoiceComparison = 1

bds$Reject = ifelse(bds$WorkerId %in% whitelist, "", 
                    ifelse(bds$temp %in% data$temp, 
                           ifelse(!(bds$temp %in% reject$temp), "",
                                  "I'm sorry, but the data collected indicates you did not listen to the audio for sufficient time to complete the task properly"),
                           "I'm sorry, but we could not find a complete submission with your information in our system."))
bds$Approve = ifelse((bds$temp %in% data$temp & bds$Reject == "") |
                       bds$WorkerId %in% whitelist, "x", "")
bds$UPDATE.CompletedVoiceComparison = 1

bjan2019$Reject = ifelse(bjan2019$WorkerId %in% whitelist, "", 
                    ifelse(bjan2019$WorkerId %in% data$URL.Variable..mid, 
                           ifelse(!(bjan2019$WorkerId %in% reject$URL.Variable..mid), "",
                                  "I'm sorry, but the data collected indicates you did not listen to the audio for sufficient time to complete the task properly"),
                           "I'm sorry, but we could not find a complete submission with your information in our system."))
bjan2019$Approve = ifelse((bjan2019$WorkerId %in% data$URL.Variable..mid & bjan2019$Reject == "") |
                           bjan2019$WorkerId %in% whitelist, "x", "")
bjan2019$UPDATE.CompletedVoiceComparison = 1

bapril2019$Reject = ifelse(bapril2019$WorkerId %in% whitelist, "", 
                         ifelse(bapril2019$WorkerId %in% data$URL.Variable..mid, 
                                ifelse(!(bapril2019$WorkerId %in% reject$URL.Variable..mid), "",
                                       "I'm sorry, but the data collected indicates you did not listen to the audio for sufficient time to complete the task properly"),
                                "I'm sorry, but we could not find a complete submission with your information in our system."))
bapril2019$Approve = ifelse((bapril2019$WorkerId %in% data$URL.Variable..mid & bapril2019$Reject == "") |
                            bapril2019$WorkerId %in% whitelist, "x", "")
bapril2019$UPDATE.CompletedVoiceComparison = 1

bapril2019j$Reject = ifelse(bapril2019j$WorkerId %in% whitelist, "", 
                           ifelse(bapril2019j$WorkerId %in% data$URL.Variable..mid, 
                                  ifelse(!(bapril2019j$WorkerId %in% reject$URL.Variable..mid), "",
                                         "I'm sorry, but the data collected indicates you did not listen to the audio for sufficient time to complete the task properly"),
                                  "I'm sorry, but we could not find a complete submission with your information in our system."))
bapril2019j$Approve = ifelse((bapril2019j$WorkerId %in% data$URL.Variable..mid & bapril2019j$Reject == "") |
                              bapril2019j$WorkerId %in% whitelist, "x", "")
bapril2019j$UPDATE.CompletedVoiceComparison = 1

bos$Reject = ifelse(bos$WorkerId %in% whitelist, "", 
                            ifelse(bos$WorkerId %in% data$URL.Variable..mid, 
                                   ifelse(!(bos$WorkerId %in% reject$URL.Variable..mid), "",
                                          "I'm sorry, but the data collected indicates you did not listen to the audio for sufficient time to complete the task properly"),
                                   "I'm sorry, but we could not find a complete submission with your information in our system."))
bos$Approve = ifelse((bos$WorkerId %in% data$URL.Variable..mid & bos$Reject == "") |
                       bos$WorkerId %in% whitelist, "x", "")
bos$UPDATE.CompletedVoiceComparison = 1

b10 = b10[,-c(31)]
b40 = b40[,-c(31)]
b200 = b200[,-c(32)]
bjo = bjo[,-c(31)]
bds = bds[,-c(31)]
bjan2019 = bjan2019[,-c(31)]
bapril2019 = bapril2019[,-c(31)]
bapril2019j = bapril2019j[,-c(31)]
bos = bos[,-c(31)]

write.csv(b10, file = "batch10-submission.csv")
write.csv(b40, file = "batch40-submission.csv")
write.csv(b200, file = "batch200-submission.csv")
write.csv(bjo, file = "batchjo-submission.csv")
write.csv(bds, file = "batchds-submission.csv")
write.csv(bjan2019, file = "batchjan2019-submission.csv")
write.csv(bapril2019, file = "batchapril2019-submission.csv")
write.csv(bapril2019j, file = "batchapril2019j-submission.csv")
write.csv(bos, file = "batchos-submission.csv")

# Remove rejected ----
approved10 = b10[which(b10$AssignmentStatus == "Approved" |
                         (b10$AssignmentStatus == "Submitted" & b10$Approve == "x")),]
approved40 = b40[which(b40$AssignmentStatus == "Approved" |
                         (b40$AssignmentStatus == "Submitted" & b40$Approve == "x")),]
approved200 = b200[which(b200$AssignmentStatus == "Approved" |
                           (b200$AssignmentStatus == "Submitted" & b200$Approve == "x")),]
approvedjo = bjo[which(bjo$AssignmentStatus == "Approved" |
                           (bjo$AssignmentStatus == "Submitted" & bjo$Approve == "x")),]
approvedds = bds[which(bds$AssignmentStatus == "Approved" |
                         (bds$AssignmentStatus == "Submitted" & bds$Approve == "x")),]
approvedjan2019 = bjan2019[which(bjan2019$AssignmentStatus == "Approved" |
                         (bjan2019$AssignmentStatus == "Submitted" & bjan2019$Approve == "x")),]
approvedapril2019 = bapril2019[which(bapril2019$AssignmentStatus == "Approved" |
                                   (bapril2019$AssignmentStatus == "Submitted" & bapril2019$Approve == "x")),]
approvedapril2019j = bapril2019j[which(bapril2019j$AssignmentStatus == "Approved" |
                                       (bapril2019j$AssignmentStatus == "Submitted" & bapril2019j$Approve == "x")),]
approvedos = bos[which(bos$AssignmentStatus == "Approved" |
                                         (bos$AssignmentStatus == "Submitted" & bos$Approve == "x")),]

approved = rbind(approved10, approved40, approved200[-c(29)], approvedjo, approvedds, approvedjan2019, approvedapril2019, approvedapril2019j, approvedos)

data = data[ which(data$URL.Variable..mid %in% approved$WorkerId),]

# Cleanup ----
remove(approved)
remove(reject)
remove(approved10)
remove(approved40)
remove(approved200)
remove(approvedjo)
remove(approvedds)
remove(approvedjan2019)
remove(approvedapril2019)
remove(approvedapril2019j)
remove(approvedos)
remove(b10)
remove(b200)
remove(b40)
remove(bjo)
remove(bds)
remove(bjan2019)
remove(bapril2019)
remove(bapril2019j)
remove(bos)

