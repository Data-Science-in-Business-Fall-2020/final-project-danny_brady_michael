# Finding the Best Selectino of Variables for Each Subset 

# Read in Data
eat <- eat_concat
drops <- drops_concat
targets <- targets_concat


setwd('C:/Users/bradi/OneDrive/Documents/Fall 2020/Stat 495R/final-project-danny_brady_michael')
eat <- read.csv("eat_concat.csv", header = TRUE, sep = ",")
drops <- read.csv("drops_concat.csv", header = TRUE, sep = ",")
targets <- read.csv("targets_concat.csv", header = TRUE, sep = ",")

# Assemble Relevant columns in Dataset for Total Sharks
complete_variables_total <- c("Saury","Blue_Runner","Squid","Mackerel","Herring","Sardine",
                         "Mazuri_Vitamins","Garlic","Salmon","Bonito","Bluefish","Mahi","Goggle_Eye",
                         "Humbolt_Squid","Temperature","covid","light_training","GroupFeed","Varied_Target",
                          "Day_of_week","Month","Day","Total")

eat_total <- eat[, complete_variables_total]
head(eat_total)

drops_total <- drops[, complete_variables_total]
head(drops_total)

targets_total <- targets[,complete_variables_total]
head(targets_total)

# Find best selection for All Sharks Pieces Eaten
full.model <- glm( Total ~ ., family="poisson", data=eat_total)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
eat_best_total <- best.lm
eat_best_total

# Find best selection for All Sharks Pieces Dropped
full.model <- glm( Total ~ ., family="poisson", data=drops_total)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
drops_best_total <- best.lm
drops_best_total

# Find best selection for All Sharks Number of Targets
full.model <- glm( Total ~ ., family="poisson", data=targets_total)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
targets_best_total <- best.lm
targets_best_total

# Assemble Relevant columns in Dataset for Sandbars
complete_variables_SS <- c("Saury","Blue_Runner","Squid","Mackerel","Herring","Sardine",
                         "Mazuri_Vitamins","Garlic","Salmon","Bonito","Bluefish","Mahi","Goggle_Eye",
                         "Humbolt_Squid","Temperature","covid","light_training","GroupFeed","Varied_Target",
                          "Day_of_week","Month","Day","All_SS")

eat_SS <- eat[, complete_variables_SS]
head(eat_SS)

drops_SS <- drops[, complete_variables_SS]
head(drops_SS)

targets_SS <- targets[,complete_variables_SS]
head(targets_SS)

# Find best selection for SS Sharks Pieces Eaten
full.model <- glm( All_SS ~ ., family="poisson", data=eat_SS)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
eat_best_SS <- best.lm
eat_best_SS

# Find best selection for SS Sharks Pieces Dropped
full.model <- glm( All_SS ~ ., family="poisson", data=drops_SS)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
drops_best_SS <- best.lm
drops_best_SS

# Find best selection for SS Sharks Number of Targets
full.model <- glm( All_SS ~ ., family="poisson", data=targets_SS)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
targets_best_SS <- best.lm
targets_best_SS



# Assemble Relevant columns in Dataset for Black Tips
complete_variables_BT <- c("Saury","Blue_Runner","Squid","Mackerel","Herring","Sardine",
                         "Mazuri_Vitamins","Garlic","Salmon","Bonito","Bluefish","Mahi","Goggle_Eye",
                         "Humbolt_Squid","Temperature","covid","light_training","GroupFeed","Varied_Target",
                          "Day_of_week","Month","Day","All_BT")

eat_BT <- eat[, complete_variables_BT]
head(eat_BT)

drops_BT <- drops[, complete_variables_BT]
head(drops_BT)

targets_BT <- targets[,complete_variables_BT]
head(targets_BT)

# Find best selection for BT Sharks Pieces Eaten
full.model <- glm( All_BT ~ ., family="poisson", data=eat_BT)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
eat_best_BT <- best.lm
eat_best_BT

# Find best selection for BT Sharks Pieces Dropped
full.model <- glm( All_BT ~ ., family="poisson", data=drops_BT)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
drops_best_BT <- best.lm
drops_best_BT

# Find best selection for BT Sharks Number of Targets
full.model <- glm( All_BT ~ ., family="poisson", data=targets_BT)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
targets_best_BT <- best.lm
targets_best_BT


# Assemble Relevant columns in Dataset for Gray Reefs
complete_variables_GR <- c("Saury","Blue_Runner","Squid","Mackerel","Herring","Sardine",
                         "Mazuri_Vitamins","Garlic","Salmon","Bonito","Bluefish","Mahi","Goggle_Eye",
                         "Humbolt_Squid","Temperature","covid","light_training","GroupFeed","Varied_Target",
                          "Day_of_week","Month","Day","All_GR")

eat_GR <- eat[, complete_variables_GR]
head(eat_GR)

drops_GR <- drops[, complete_variables_GR]
head(drops_GR)

targets_GR <- targets[,complete_variables_GR]
head(targets_GR)

# Find best selection for GR Sharks Pieces Eaten
full.model <- glm( All_GR ~ ., family="poisson", data=eat_GR)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
eat_best_GR <- best.lm
eat_best_GR

# Find best selection for GR Sharks Pieces Dropped
full.model <- glm( All_GR ~ ., family="poisson", data=drops_GR)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
drops_best_GR <- best.lm
drops_best_GR

# Find best selection for GR Sharks Number of Targets
full.model <- glm( All_GR ~ ., family="poisson", data=targets_GR)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
targets_best_GR <- best.lm
targets_best_GR


# Assemble Relevant columns in Dataset for Male Sharks
complete_variables_M <- c("Saury","Blue_Runner","Squid","Mackerel","Herring","Sardine",
                         "Mazuri_Vitamins","Garlic","Salmon","Bonito","Bluefish","Mahi","Goggle_Eye",
                         "Humbolt_Squid","Temperature","covid","light_training","GroupFeed","Varied_Target",
                          "Day_of_week","Month","Day","male")

eat_M <- eat[, complete_variables_M]
head(eat_M)

drops_M <- drops[, complete_variables_M]
head(drops_M)

targets_M <- targets[,complete_variables_M]
head(targets_M)

# Find best selection for Male Sharks Pieces Eaten
full.model <- glm( male ~ ., family="poisson", data=eat_M)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
eat_best_M <- best.lm
eat_best_M

# Find best selection for Male Sharks Pieces Dropped
full.model <- glm( male ~ ., family="poisson", data=drops_M)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
drops_best_M <- best.lm
drops_best_M

# Find best selection for Male Sharks Number of Targets
full.model <- glm( male ~ ., family="poisson", data=targets_M)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
targets_best_M <- best.lm
targets_best_M


# Assemble Relevant columns in Dataset for Female Sharks
complete_variables_F <- c("Saury","Blue_Runner","Squid","Mackerel","Herring","Sardine",
                         "Mazuri_Vitamins","Garlic","Salmon","Bonito","Bluefish","Mahi","Goggle_Eye",
                         "Humbolt_Squid","Temperature","covid","light_training","GroupFeed","Varied_Target",
                          "Day_of_week","Month","Day","female")

eat_F <- eat[, complete_variables_F]
head(eat_F)

drops_F <- drops[, complete_variables_F]
head(drops_F)

targets_F <- targets[,complete_variables_F]
head(targets_F)

# Find best selection for Female Sharks Pieces Eaten
full.model <- glm( female ~ ., family="poisson", data=eat_F)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
eat_best_F <- best.lm
eat_best_F

# Find best selection for Female Sharks Pieces Dropped
full.model <- glm( female ~ ., family="poisson", data=drops_F)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
drops_best_F <- best.lm
drops_best_F

# Find best selection for Female Sharks Number of Targets
full.model <- glm( female ~ ., family="poisson", data=targets_F)
empty.model <- update(full.model, . ~ 1)
best.lm <- step(empty.model,scope=list(lower=.~1, upper=formula(full.model)), direction = "both")
targets_best_F <- best.lm
targets_best_F
