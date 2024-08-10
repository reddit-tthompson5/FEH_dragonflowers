rm(list = ls())
DFs = read.csv("FEH_zeroed_DFs.csv")


# Setup/User Input --------------------------------------------------------

start_date = "2023-08-11" # inclusive
end_date = "2024-08-10" # inclusive

# dealing with AR
DFs$Number[DFs$Source == "AR Def"] = 10 # AR Defense reward
DFs$Number[DFs$Source == "AR Rank"] = 36 # AR Rank reward
DFs$Number[DFs$Source == "AR Tier"] = 46 # AR Tier reward

# dealing with MS
MS = (DFs$Source == "Mjolnir's Strike")
DFs$Number[MS & (DFs$Type == "Fly" | DFs$Type == "Arm")] = 52.5 # MS rewards
DFs$Number[MS & (DFs$Type == "Inf" | DFs$Type == "Cav")] = 52.5

# dealing with RB
DFs$Number[DFs$Source == "Resonant Battles"] = 37.5 # Resonant Battles reward

# dealing with SD-R and SD-S
# DFs$Number for SD-R and SD-S are already set to values for 0 glory. 
# Adjust accordingly. 
# DFs$Number[DFs$Source == "SD-R Tier"] = 80 # SD-R Tier reward
# DFs$Number[DFs$Source == "SD-R Rank"] = 120 # SD-R Rank reward
# DFs$Number[DFs$Source == "SD-S Tier"] = 80 # SD-S Tier reward
# DFs$Number[DFs$Source == "SD-S Rank"] = 120 # SD-S Rank reward

# OG Summoner Duels
# DFs$Number[DFs$Source == "Summoner Duels"] = 0 # un-comment if SD is not played

# AR Resort
inf_weekly = 0
arm_weekly = 0
cav_weekly = 0
fly_weekly = 0
resort_weekly = c(inf_weekly, arm_weekly, cav_weekly, fly_weekly)

# dealing with Allegiance Battles
DFs$Number[DFs$Source == "Allegiance Battles"] = 34 # Allegiance Battles reward

# dealing with AA+ Rank
DFs$Number[DFs$Source == "AA+ Rank"] = 42 # Arena Assault+ rank reward


# Creating metadata -------------------------------------------------------

Source = c("AR Def", "AR Rank", "AR Tier", "Mjolnir's Strike",
           "Mjolnir's Strike LV.", "Resonant Battles", "SD-R Rank", "SD-R Tier",
           "SD-S Rank", "SD-S Tier", "Allegiance Battles", "AA+ Chain", "AA+ Rank", 
           "Grail Units", "Binding Worlds", "Hall of Forms", "Revival HoF",
           "Pawns of Loki", "Heroes Journey", "Lost Lore", "Type Quests", 
           "Summoner Duels Start", "Summoner Duels", "Frontline Phalanx", 
           "Rokkr Sieges", "Seer's Snare", "Affinity Auto-Battles",
           "United Warfront", "New Unit Quest/Login/Book", 
           "Anniversary Bonus", "Golden Week Bonus", "Book Midpoint Bonus", 
           "Half-Anniversary Bonus", "CYL Bonus", "1000th Hero",
           "New Hero Type Celebration", "Black Friday Bonus", 
           "Book Begins Bonus", "New Year's Bonus")

celebratory = Source %in% c("Anniversary Bonus", "Golden Week Bonus", 
                            "Book Midpoint Bonus", "Half-Anniversary Bonus", 
                            "CYL Bonus", "1000th Hero",
                            "New Hero Type Celebration", "Black Friday Bonus", 
                            "Book Begins Bonus", "New Year's Bonus")

fixed = Source %in% c("Mjolnir's Strike LV.", "AA+ Chain", "Grail Units", 
                      "Binding Worlds", "Hall of Forms", "Revival HoF", 
                      "Pawns of Loki", "Heroes Journey", "Lost Lore", 
                      "Type Quests", "Summoner Duels Start", "Summoner Duels",
                      "Frontline Phalanx", "Rokkr Sieges", "Seer's Snare",
                      "Affinity Auto-Battles", "United Warfront", 
                      "New Unit Quest/Login/Book")

variable = Source %in% c( "AR Def", "AR Rank", "AR Tier", "Mjolnir's Strike",
                          "Resonant Battles", "SD-R Rank", "SD-R Tier",
                          "SD-S Rank", "SD-S Tier", "Allegiance Battles", 
                          "AA+ Rank")

cats = length(Source)

Total = 0*c(1:cats)
Total_Inf = 0*c(1:cats)
Total_Armor = 0*c(1:cats)
Total_Cav = 0*c(1:cats)
Total_Flyer = 0*(1:cats)
Times_This_Year = 0*c(1:cats)
Amount = 0*c(1:cats)

# date range
currentYear = DFs$Date >= as.Date(start_date) & DFs$Date <=as.Date(end_date)
# start date and end date of when to consider for estimate

yearDFs = sum(DFs$Number[currentYear])
yearInfDFs = sum(DFs$Number[intersect(which(currentYear), which(DFs$Type == "Inf"))])
yearArmorDFs = sum(DFs$Number[intersect(which(currentYear), which(DFs$Type == "Arm"))])
yearCavDFs = sum(DFs$Number[intersect(which(currentYear), which(DFs$Type == "Cav"))])
yearFlyerDFs = sum(DFs$Number[intersect(which(currentYear), which(DFs$Type == "Fly"))])


#computing stats for category "AR Def"
curIdx = which(Source == "AR Def")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "AR Def"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "AR Def"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "AR Def"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "AR Def"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "AR Def"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "AR Def")))
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "AR Rank"
curIdx = which(Source == "AR Rank")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "AR Rank"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "AR Rank"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "AR Rank"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "AR Rank"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "AR Rank"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "AR Rank")))
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "AR Tier"
curIdx = which(Source == "AR Tier")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "AR Tier"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "AR Tier"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "AR Tier"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "AR Tier"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "AR Tier"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "AR Tier")))
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Mjolnir's Strike"
curIdx = which(Source == "Mjolnir's Strike")
Total[curIdx] = sum(DFs$Number[currentYear & DFs$Source == "Mjolnir's Strike"])
Total_Inf[curIdx] = sum(DFs$Number[currentYear & DFs$Type == "Inf" &
                                     DFs$Source == "Mjolnir's Strike"])
Total_Armor[curIdx] = sum(DFs$Number[currentYear & DFs$Type == "Arm" &
                                       DFs$Source == "Mjolnir's Strike"])
Total_Cav[curIdx] = sum(DFs$Number[currentYear & DFs$Type == "Cav" &
                                     DFs$Source == "Mjolnir's Strike"])
Total_Flyer[curIdx] = sum(DFs$Number[currentYear & DFs$Type == "Fly" &
                                       DFs$Source == "Mjolnir's Strike"])
Times_This_Year[curIdx] = sum(currentYear & DFs$Source == "Mjolnir's Strike")
Amount[curIdx] = sum(DFs$Number[currentYear & 
                                  DFs$Source == "Mjolnir's Strike"])/Times_This_Year[curIdx]


#computing stats for category "Mjolnir's Strike LV."
curIdx = which(Source == "Mjolnir's Strike LV.")
Total[curIdx] = sum(DFs$Number[currentYear & DFs$Source == "Mjolnir's Strike LV. 30"])
Total_Inf[curIdx] = sum(DFs$Number[currentYear & DFs$Type == "Inf" &
                                     DFs$Source == "Mjolnir's Strike LV. 30"])
Total_Armor[curIdx] = sum(DFs$Number[currentYear & DFs$Type == "Arm" &
                                       DFs$Source == "Mjolnir's Strike LV. 30"])
Total_Cav[curIdx] = sum(DFs$Number[currentYear & DFs$Type == "Cav" &
                                     DFs$Source == "Mjolnir's Strike LV. 30"])
Total_Flyer[curIdx] = sum(DFs$Number[currentYear & DFs$Type == "Fly" &
                                       DFs$Source == "Mjolnir's Strike LV. 30"])
Times_This_Year[curIdx] = sum(currentYear & DFs$Source == "Mjolnir's Strike")
Amount[curIdx] = sum(DFs$Number[currentYear & 
                                  DFs$Source == "Mjolnir's Strike LV. 30"])/Times_This_Year[curIdx]


#computing stats for category "Resonant Battles"
curIdx = which(Source == "Resonant Battles")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "Resonant Battles"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "Resonant Battles"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "Resonant Battles"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "Resonant Battles"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "Resonant Battles"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "Resonant Battles")))
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "SD-R Rank"
curIdx = which(Source == "SD-R Rank")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "SD-R Rank"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "SD-R Rank"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "SD-R Rank"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "SD-R Rank"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "SD-R Rank"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "SD-R Rank")))
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "SD-R Tier"
curIdx = which(Source == "SD-R Tier")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "SD-R Tier"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "SD-R Tier"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "SD-R Tier"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "SD-R Tier"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "SD-R Tier"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "SD-R Tier")))
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "SD-S Rank"
curIdx = which(Source == "SD-S Rank")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "SD-S Rank"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "SD-S Rank"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "SD-S Rank"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "SD-S Rank"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "SD-S Rank"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "SD-S Rank")))
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "SD-S Tier"
curIdx = which(Source == "SD-S Tier")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "SD-S Tier"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "SD-S Tier"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "SD-S Tier"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "SD-S Tier"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "SD-S Tier"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "SD-S Tier")))
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Allegiance Battles"
curIdx = which(Source == "Allegiance Battles")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "Allegiance Battles"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "Allegiance Battles"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "Allegiance Battles"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "Allegiance Battles"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "Allegiance Battles"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "Allegiance Battles")))
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "AA+ Chain"
curIdx = which(Source == "AA+ Chain")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "AA+ Chain"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "AA+ Chain"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "AA+ Chain"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "AA+ Chain"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "AA+ Chain"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "AA+ Chain")))
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "AA+ Rank"
curIdx = which(Source == "AA+ Rank")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "AA+ Rank"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "AA+ Rank"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "AA+ Rank"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "AA+ Rank"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "AA+ Rank"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "AA+ Rank")))
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Grail Units"
curIdx = which(Source == "Grail Units")
grailUnits = intersect(which(currentYear), which(DFs$Source == "TT-New Unit" |
                                                   DFs$Source == "GHB-New Unit"))
Total[curIdx] = sum(DFs$Number[grailUnits])
Total_Inf[curIdx] = sum(DFs$Number[intersect(grailUnits, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(grailUnits, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(grailUnits, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(grailUnits, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = length(grailUnits)
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Binding Worlds"
curIdx = which(Source == "Binding Worlds")
BW = intersect(which(currentYear), which(DFs$Source == "Binding Worlds Enclosures" |
                                           DFs$Source == "Binding Worlds Daily"))
Total[curIdx] = sum(DFs$Number[BW])
Total_Inf[curIdx] = sum(DFs$Number[intersect(BW, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(BW, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(BW, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(BW, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear), 
                                           which(DFs$Source == "Binding Worlds Enclosures")))
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Hall of Forms"
curIdx = which(Source == "Hall of Forms")
HoF = intersect(which(currentYear), which(DFs$Source == "Hall of Forms Chambers" |
                                            DFs$Source == "Hall of Forms Daily"))
Total[curIdx] = sum(DFs$Number[HoF])
Total_Inf[curIdx] = sum(DFs$Number[intersect(HoF, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(HoF, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(HoF, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(HoF, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = length(HoF)/4
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]

#computing stats for category "Revival HoF"
curIdx = which(Source == "Revival HoF")
HoFRevival = intersect(which(currentYear), which(DFs$Source == "Revival: Hall of Forms"))
Total[curIdx] = sum(DFs$Number[HoFRevival])
Total_Inf[curIdx] = sum(DFs$Number[intersect(HoFRevival, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(HoFRevival, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(HoFRevival, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(HoFRevival, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = length(HoFRevival)
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Pawns of Loki"
curIdx = which(Source == "Pawns of Loki")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "Pawns of Loki"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "Pawns of Loki"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "Pawns of Loki"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "Pawns of Loki"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "Pawns of Loki"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "Pawns of Loki")))/4
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Heroes Journey"
curIdx = which(Source == "Heroes Journey")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "Heroes Journey"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "Heroes Journey"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "Heroes Journey"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "Heroes Journey"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "Heroes Journey"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "Heroes Journey")))/4
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Lost Lore" (including Lost Lore: Spoils)
curIdx = which(Source == "Lost Lore")
Total[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                         which(DFs$Source == "Lost Lore"))])
Total_Inf[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Inf" & DFs$Source == "Lost Lore"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Arm" & DFs$Source == "Lost Lore"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                             which(DFs$Type == "Cav" & DFs$Source == "Lost Lore"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(which(currentYear), 
                                               which(DFs$Type == "Fly" & DFs$Source == "Lost Lore"))])
Times_This_Year[curIdx] = length(intersect(which(currentYear),
                                           which(DFs$Source == "Lost Lore")))
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Type Quests"
curIdx = which(Source == "Type Quests")
typeQuests = intersect(which(currentYear), which(DFs$Source == "Q-Infantry Assault" |
                                                   DFs$Source == "Q-Infantry Clash" | DFs$Source == "Q-Infantry Strike" |
                                                   DFs$Source == "Q-Armor Assault" | DFs$Source == "Q-Armor Clash" | 
                                                   DFs$Source == "Q-Armored Strike" | DFs$Source == "Q-Cavalry Assault" |
                                                   DFs$Source == "Q-Cavalry Clash" | DFs$Source == "Q-Cavalry Strike" |
                                                   DFs$Source == "Q-Flying Assault" | DFs$Source == "Q-Flying Clash" | 
                                                   DFs$Source == "Q-Flying Strike" ))
Total[curIdx] = sum(DFs$Number[typeQuests])
Total_Inf[curIdx] = sum(DFs$Number[intersect(typeQuests, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(typeQuests, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(typeQuests, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(typeQuests, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = length(typeQuests)
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Summoner Duels Start"
curIdx = which(Source == "Summoner Duels Start")
SDStart = intersect(which(currentYear), which(DFs$Source == "Summoner Duels" 
                                              & DFs$Date == "2021-12-06"))
Total[curIdx] = sum(DFs$Number[SDStart])
Total_Inf[curIdx] = sum(DFs$Number[intersect(SDStart, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(SDStart, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(SDStart, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(SDStart, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = 1
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Summoner Duels"
curIdx = which(Source == "Summoner Duels")
SD = intersect(which(currentYear), which(DFs$Source == "Summoner Duels" 
                                         & DFs$Date != "2021-12-06"))
Total[curIdx] = sum(DFs$Number[SD])
Total_Inf[curIdx] = sum(DFs$Number[intersect(SD, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(SD, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(SD, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(SD, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = length(SD)
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Frontline Phalanx"
curIdx = which(Source == "Frontline Phalanx")
FP= intersect(which(currentYear), which(DFs$Source == "Frontline Phalanx"))
Total[curIdx] = sum(DFs$Number[FP])
Total_Inf[curIdx] = sum(DFs$Number[intersect(FP, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(FP, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(FP, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(FP, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = length(FP)
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Rokkr Sieges"
curIdx = which(Source == "Rokkr Sieges")
RS = intersect(which(currentYear), which(DFs$Source == "Rokkr Sieges"))
Total[curIdx] = sum(DFs$Number[RS])
Total_Inf[curIdx] = sum(DFs$Number[intersect(RS, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(RS, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(RS, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(RS, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = length(RS)/3
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]

#computing stats for category "Seer's Snare"
curIdx = which(Source == "Seer's Snare")
SS = intersect(which(currentYear), which(DFs$Source == "Seer's Snare Daily" | 
                                           DFs$Source == "Seer's Snare Rifts"))
Total[curIdx] = sum(DFs$Number[SS])
Total_Inf[curIdx] = sum(DFs$Number[intersect(SS, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(SS, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(SS, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(SS, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = length(SS)/3
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]

#computing stats for category "Affinity Auto-Battles"
curIdx = which(Source == "Affinity Auto-Battles")
AAB = intersect(which(currentYear), which(DFs$Source == "Affinity Auto-Battles Daily" | 
                                           DFs$Source == "Affinity Auto-Battles Score"))
Total[curIdx] = sum(DFs$Number[AAB])
Total_Inf[curIdx] = sum(DFs$Number[intersect(AAB, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(AAB, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(AAB, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(AAB, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = length(AAB)/4
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]

#computing stats for category "United Warfront"
curIdx = which(Source == "United Warfront")
UW = intersect(which(currentYear), which(DFs$Source == "United Warfront"))
Total[curIdx] = sum(DFs$Number[UW])
Total_Inf[curIdx] = sum(DFs$Number[intersect(UW, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(UW, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(UW, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(UW, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = length(UW)/4
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]



#computing stats for category "New Unit Quest/Login/Book"
curIdx = which(Source == "New Unit Quest/Login/Book")
newUnits = intersect(which(currentYear), which(DFs$Source == "Unit Login Bonus" |
                                                 DFs$Source == "B-New Unit" | DFs$Source == "Q-New Unit" |
                                                 DFs$Source == "AHR-New Unit" | DFs$Source == "Engage Login"))
Total[curIdx] = sum(DFs$Number[newUnits])
Total_Inf[curIdx] = sum(DFs$Number[intersect(newUnits, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(newUnits, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(newUnits, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(newUnits, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = length(newUnits)
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Anniversary Bonus"
curIdx = which(Source == "Anniversary Bonus")
anniBonus = intersect(which(currentYear), which(DFs$Source == "Q-Anniversary"|
                                                  DFs$Source == "Anniversary Login" |
                                                  DFs$Source == "Tempest Trials (Anniversary)"))
Total[curIdx] = sum(DFs$Number[anniBonus])
Total_Inf[curIdx] = sum(DFs$Number[intersect(anniBonus, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(anniBonus, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(anniBonus, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(anniBonus, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = 1
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Golden Week Bonus"
curIdx = which(Source == "Golden Week Bonus")
GWBonus = intersect(which(currentYear), which(DFs$Source == "Golden Week Login" |
                                                DFs$Source == "Rokkr Sieges (GW)" |
                                                DFs$Source == "Q-Rokkr Sieges"))
Total[curIdx] = sum(DFs$Number[GWBonus])
Total_Inf[curIdx] = sum(DFs$Number[intersect(GWBonus, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(GWBonus, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(GWBonus, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(GWBonus, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = 1
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Book Midpoint Bonus"
curIdx = which(Source == "Book Midpoint Bonus")
midBonus = intersect(which(currentYear), which(DFs$Source == "Midpoint Login"))
Total[curIdx] = sum(DFs$Number[midBonus])
Total_Inf[curIdx] = sum(DFs$Number[intersect(midBonus, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(midBonus, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(midBonus, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(midBonus, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = 1
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Half-Anniversary Bonus"
curIdx = which(Source == "Half-Anniversary Bonus")
halfBonus = intersect(which(currentYear), which(DFs$Source == "Half-Anniversary Login"))
Total[curIdx] = sum(DFs$Number[halfBonus])
Total_Inf[curIdx] = sum(DFs$Number[intersect(halfBonus, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(halfBonus, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(halfBonus, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(halfBonus, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = 1
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "CYL Bonus"
curIdx = which(Source == "CYL Bonus")
CYLBonus = intersect(which(currentYear), which(DFs$Source == "CYL Login" |
                                                 DFs$Source == "Forging Bonds (CYL)"))
Total[curIdx] = sum(DFs$Number[CYLBonus])
Total_Inf[curIdx] = sum(DFs$Number[intersect(CYLBonus, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(CYLBonus, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(CYLBonus, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(CYLBonus, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = 1
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "New Hero Type Celebration"
curIdx = which(Source == "New Hero Type Celebration")
ascBonus = intersect(which(currentYear), which(DFs$Source == "Q-Ascended Heroes" |
                                                 DFs$Source == "Ascended Heroes Login" | 
                                                 DFs$Source == "Rearmed Heroes Login" |
                                                 DFs$Source == "Attuned Heroes Login"))
Total[curIdx] = sum(DFs$Number[ascBonus])
Total_Inf[curIdx] = sum(DFs$Number[intersect(ascBonus, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(ascBonus, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(ascBonus, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(ascBonus, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = 1
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Black Friday Bonus"
curIdx = which(Source == "Black Friday Bonus")
BFBonus = intersect(which(currentYear), which(DFs$Source == "Q-Black Friday"))
Total[curIdx] = sum(DFs$Number[BFBonus])
Total_Inf[curIdx] = sum(DFs$Number[intersect(BFBonus, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(BFBonus, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(BFBonus, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(BFBonus, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = 1
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "Book Begins Bonus"
curIdx = which(Source == "Book Begins Bonus")
beginBonus = intersect(which(currentYear), which(DFs$Source == "Book Begins Login" |
                                                   DFs$Source == "Q-Book Begins"))
Total[curIdx] = sum(DFs$Number[beginBonus])
Total_Inf[curIdx] = sum(DFs$Number[intersect(beginBonus, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(beginBonus, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(beginBonus, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(beginBonus, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = 1
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]


#computing stats for category "New Year's Bonus"
curIdx = which(Source == "New Year's Bonus")
NYBonus = intersect(which(currentYear), which(DFs$Source == "Q-New Year"))
Total[curIdx] = sum(DFs$Number[NYBonus])
Total_Inf[curIdx] = sum(DFs$Number[intersect(NYBonus, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(NYBonus, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(NYBonus, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(NYBonus, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = 1
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]

#computing stats for category "1000th Hero"
curIdx = which(Source == "1000th Hero")
TH = intersect(which(currentYear), which(DFs$Source == "1000th Hero Login"))
Total[curIdx] = sum(DFs$Number[TH])
Total_Inf[curIdx] = sum(DFs$Number[intersect(TH, which(DFs$Type == "Inf"))])
Total_Armor[curIdx] = sum(DFs$Number[intersect(TH, which(DFs$Type == "Arm"))])
Total_Cav[curIdx] = sum(DFs$Number[intersect(TH, which(DFs$Type == "Cav"))])
Total_Flyer[curIdx] = sum(DFs$Number[intersect(TH, which(DFs$Type == "Fly"))])
Times_This_Year[curIdx] = 1
Amount[curIdx] = Total[curIdx]/Times_This_Year[curIdx]



# Formatting/Printing Results ---------------------------------------------

Amount = round(Amount)
metaData = data.frame(Amount, Times_This_Year, Source, Total, Total_Inf, Total_Armor, 
                      Total_Cav, Total_Flyer)
if(sum(resort_weekly) != 0)
{
  Tuesdays = 
    sum(weekdays(seq(as.Date(start_date), as.Date(end_date), "days")) == "Tuesday")
  total_resort = Tuesdays*resort_weekly
  ar_resort = list(sum(resort_weekly), Tuesdays, "AR Resort", sum(total_resort),
                   total_resort[1], total_resort[2], total_resort[3], total_resort[4])
  metaData = rbind(metaData, ar_resort)
}
colnames(metaData) = c("Amount", "Times", "Source", "Total", "Infantry",
                       "Armored", "Cavalry", "Flying")
Totals = list(NA, NA, "Totals", sum(metaData$Total), sum(metaData$Infantry), 
              sum(metaData$Armored), sum(metaData$Cavalry), 
              sum(metaData$Flying))
metaData = rbind(metaData, Totals)

print(metaData)
print(sum(Total))

# write.csv(metaData, "FEH_DFs_estimate.csv", row.names = FALSE)
  # uncomment to write meta-data to a csv file in your working directory

# making a graph of yearly gameplay DFs
library(stringr)
start_date = as.Date("2021-05-10")
end_date = as.Date("2023-08-10")
DFsYTD = c()
dates = c()

date = start_date
while(date <= end_date)
{
  curr_year = as.integer(str_split_i(date, pattern="-", 1))
  curr_month = as.integer(str_split_i(date, pattern="-", 2))
  curr_day = as.integer(str_split_i(date, pattern="-", 3))
  int_year = curr_year+1
  
  date2 = as.Date(paste(int_year, curr_month, curr_day, sep="-"))
  range = as.Date(DFs$Date) > date & as.Date(DFs$Date) <= date2 
  DFsYTD = c(DFsYTD, sum(DFs$Number[range]))
  dates = c(dates, date2)
  
  int_month = curr_month+1
  if(int_month == 13)
  {
    int_month = 1
    curr_year = curr_year+1
  }
  date = as.Date(paste(curr_year, int_month, curr_day, sep="-"))
  
}

plot(as.Date(dates), DFsYTD, xlab = "Dates",  
     ylab = "previous year of gameplay DFs")
# title("Player B")

# making summary table of celebratory, fixed, and variable DFs
DFsTable = data.frame(as.Date(dates), round(DFsYTD))

sum_Total = c(sum(Total[celebratory]), sum(Total[fixed]), sum(Total[variable]),
              sum(Total))
sum_Total_Inf = c(sum(Total_Inf[celebratory]), sum(Total_Inf[fixed]), 
                  sum(Total_Inf[variable]), sum(Total_Inf))
sum_Total_Armor = c(sum(Total_Armor[celebratory]), sum(Total_Armor[fixed]), 
                    sum(Total_Armor[variable]), sum(Total_Armor))
sum_Total_Cav = c(sum(Total_Cav[celebratory]), sum(Total_Cav[fixed]), 
                  sum(Total_Cav[variable]), sum(Total_Cav))
sum_Total_Flyer = c(sum(Total_Flyer[celebratory]), sum(Total_Flyer[fixed]), 
                    sum(Total_Flyer[variable]), sum(Total_Flyer))
DFs_summary = data.frame(sum_Total, sum_Total_Inf, sum_Total_Armor, 
                         sum_Total_Cav, sum_Total_Flyer)
colnames(DFs_summary) = c("Total", "Inf", "Arm", "Cav", "Fly")
row.names(DFs_summary) = c("Celebratory", "Fixed", "Variable", "Col Sums")
print(DFs_summary)