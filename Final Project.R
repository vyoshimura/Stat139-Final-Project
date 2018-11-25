getwd()
setwd("/Users/austin.sechrest/Documents/Junior Fall/Stat 139")
logs <- read.csv("reg.csv", header = TRUE)
logs <- as.data.frame(logs)
mean(logs$X3P.)
split <- split(logs, logs$TEAM)
total = list()
model <- lm(logs$X... ~ logs$OREB + logs$FGM + logs$FGA + logs$FG. + logs$X3PM + logs$X3PA + logs$X3P. + logs$FTM + logs$FTA + logs$FT. + logs$DREB + logs$AST + logs$STL + logs$BLK + logs$TOV + logs$PF + logs$EFG., data = logs)
summary(model)
model <- lm(logs$X... ~ logs$OREB + logs$FGM + logs$FGA + logs$X3PM + logs$FTM + logs$FTA + logs$DREB + logs$STL + logs$BLK + logs$TOV, data = logs)
summary(model)
model <- lm(logs$W ~ logs$FGM + logs$FTM + logs$REB + logs$STL + logs$BLK + logs$TOV + logs$PF, data = logs)
summary(model)

for (i in 1:30)
{
  logs1 <- as.data.frame(split[i])
  names(logs1)[4] <- "W"
  names(logs1)[7] <- "FGM"
  names(logs1)[10] <- "X3PM"
  names(logs1)[13] <- "FTM"
  names(logs1)[18] <- "REB"
  names(logs1)[20] <- "STL"
  names(logs1)[21] <- "BLK"
  names(logs1)[22] <- "TOV"
  names(logs1)[23] <- "PF"
  model <- lm(logs1 ~ logs1$FGM + logs1$X3PM + logs1$FTM + logs1$REB + logs1$STL + logs1$BLK + logs1$TOV + logs1$PF, data = logs)
  summary(model)
}
hist(logs$W)
hist(logs$PTS)
hist(logs$X..., breaks = 20)
