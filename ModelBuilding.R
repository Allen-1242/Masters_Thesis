library(data.table)
library(TangledFeatures)
library(counterfactuals)
library(tidyr)

#Model building: 

#Clean and load the dataset
data <- read.csv("/Users/allensunny/Downloads/qc_pub_fy2023 2.csv")
data <- as.data.table(data)

data_cleaned <- data[,c('YRMONTH','STATENAME', 'CERTHHSZ', 'FSDEPDED', 'FSMEDDED', 'FSSLTEXP','FSEARN',
                        'FSUNEARN','FSDIS', 'FSELDER','HHLDNO','HOMELESS_DED','FSBENSUPP','FSBEN', 'MED_DED_DEMO', 'FSCSDED')]


gap <- df$FSNETINC - results$net_income
summary(gap)



#Filter on Maryland, 2023 data
data_cleaned_maryland <- data_cleaned[STATENAME == 'Maryland',]
data_cleaned_maryland <- data_cleaned_maryland[YRMONTH > 202212]


data_new <- data_cleaned_maryland[,-c('STATENAME', 'YRMONTH')]
data_new$FSELDER <- as.factor(data_new$FSELDER)
data_new$FSDIS <- as.factor(data_new$FSDIS)
#data_new$FSSLTEXP <- data_new$FSSLTEXP + 557               # add FY-23 HCSUA


#Note please recheck the snap eligibility checker

results <- snap_md_eligibility(data_cleaned_maryland)
results$DENIAL_CODE <- 'NA'
results$IS_APPROVED <- 1

#Generation of the negative dataset
#Rules: We need to create a list of datapoints that are significantly different from the standard datapoints such tha
#The eligibility of the system is easiliy identifiable 






# ────────────────────────────────────────────────────────────────────────
# 0.  Load the pared-down Maryland QC positives  (14 columns, all TRUE)
# ────────────────────────────────────────────────────────────────────────
library(dplyr)

qc_pos <- data_cleaned_maryland
#Clean data
qc_pos[is.na(qc_pos)] <- 0


generate_negatives_2023 <- function(qc_pos, neg_ratio = 3L, seed = 42) {
  set.seed(seed)
  
  # 1. FY-2023 constants
  fpl_100     <- c(13590, 18310, 23030, 27750, 32470, 37190) / 12
  names(fpl_100) <- 1:6
  fpl_200     <- 2 * fpl_100
  shelter_cap <- 624
  
  # 2. Single-rule mutators (never NULL)
  mut_excess_gross <- function(r) {
    b   <- snap_md_eligibility(r)
    gap <- fpl_200[as.character(b$CERTHHSZ[1])] - b$gross[1]
    if (is.na(gap) || gap <= 0) return(r)
    r$FSEARN <- r$FSEARN + gap + sample(50:100, 1)
    r
  }
  
  mut_excess_net <- function(r) {
    b   <- snap_md_eligibility(r)
    gap <- fpl_100[as.character(b$CERTHHSZ[1])] - b$net_income[1]
    if (is.na(gap) || gap <= 0) return(r)
    r$FSUNEARN <- r$FSUNEARN + gap + sample(50:100, 1)
    r
  }
  
  mut_shelter_cap <- function(r) {
    b   <- snap_md_eligibility(r)
    gap <- shelter_cap - b$excess_shelter[1]
    if (is.na(gap) || gap <= 0) return(r)
    r$FSSLTEXP <- r$FSSLTEXP + gap + sample(50:100, 1)
    r
  }
  
  mut_asset_fail <- function(r) {
    r$FSASSET <- 4500 + sample(300:800, 1)
    r
  }
  
  mutators <- list(
    EXCESS_GROSS = mut_excess_gross,
    EXCESS_NET   = mut_excess_net,
    SHELTER_CAP  = mut_shelter_cap,
    ASSET_LIMIT  = mut_asset_fail
  )
  
  # 3. Build multi-fault negatives
  neg_list <- list()
  #sizes   <- sort(unique(qc_pos$CERTHHSZ))
  sizes   <- c(1,2,3,4,5,6)
  
  for (sz in sizes) {
    pos_sz   <- qc_pos %>% filter(CERTHHSZ == sz)
    target_n <- nrow(pos_sz) * neg_ratio
    count    <- 0L
    
    while (count < target_n) {
      base <- slice_sample(pos_sz, n = 1)
      b    <- snap_md_eligibility(base)
      
      # 3a) Figure out which rules we can still break
      applicable <- character(0)
      if (b$gross[1]    <= fpl_200[as.character(sz)]) applicable <- c(applicable, "EXCESS_GROSS")
      if (b$net_income[1] <= fpl_100[as.character(sz)]) applicable <- c(applicable, "EXCESS_NET")
      if (b$excess_shelter[1] <  shelter_cap)        applicable <- c(applicable, "SHELTER_CAP")
      # skip asset in MD if never used; otherwise:
      # if (!is.null(b$pass_asset) && b$pass_asset[1]) applicable <- c(applicable, "ASSET_LIMIT")
      
      # nothing left to break?
      if (length(applicable) == 0) next
      
      # 3b) Randomly pick 1–length(applicable) of them
      n_muts <- sample(seq_along(applicable), 1)
      chosen <- sample(applicable, n_muts, replace = FALSE)
      
      # 3c) Apply them in sequence
      cand <- base
      for (m in chosen) {
        cand <- mutators[[m]](cand)
      }
      
      # 3d) Test eligibility
      out <- snap_md_eligibility(cand)
      if (out$eligible[1]) next
      
      # 3e) Label all failed rules
      fails <- c(
        if (!out$pass_gross[1])     "EXCESS_GROSS",
        if (!out$pass_net[1])       "EXCESS_NET",
        if (out$excess_shelter[1] >= shelter_cap && !out$pass_net[1])
          "SHELTER_CAP"
        # + asset if you need it
      )
      out$DENIAL_CODE <- paste(unique(fails), collapse = "+")
      out$IS_APPROVED <- 0L
      
      neg_list[[length(neg_list) + 1]] <- out
      count <- count + 1L
    }
  }
  
  bind_rows(neg_list)
}

# ── Example usage ─────────────────────────────────────────────────────────
neg_df_2023 <- generate_negatives_2023(data_cleaned_maryland)

neg_df_2023 %>%
  select(CERTHHSZ, DENIAL_CODE, pass_gross, pass_net, excess_shelter) %>%
  slice(1:10)



# 3.  bind the two stacks ------------------------------------------------
train_df <- bind_rows(results, neg_df_2023)
train_df <- train_df %>% drop_na()


#Model testing 

#Linear testing
formula_TangledFeatures <- as.formula(paste(paste("IS_APPROVED", '~'), paste(colnames(train_df[,c("CERTHHSZ","FSDEPDED","FSMEDDED","FSSLTEXP" ,"FSEARN" ,  "FSUNEARN"   ,"FSDIS" ,"FSELDER", "IS_APPROVED")])
, collapse = "+")))
lm_TangledFeatures <- lm(formula_TangledFeatures, data = train_df)
summary(lm_TangledFeatures)


colnames(train_df)

#Class differences testing
library(Rtsne)          # install.packages("Rtsne")
library(ggplot2)
library(dplyr)
library(tidyr)


# ── 1.  pick predictors only (drop FSBEN, labels) ───────────────────────
temp <- train_df %>% filter(CERTHHSZ == 1)

X <- temp %>% select(FSEARN, FSUNEARN,
         FSDEPDED, FSMEDDED, FSSLTEXP,
         FSELDER, FSDIS)

# ── 2.  one-hot encode the two flags to numeric 0/1 cols ────────────────
X$FSELDER <- as.numeric(X$FSELDER)
X$FSDIS   <- as.numeric(X$FSDIS)

X <- X %>% drop_na()


# ── 3.  centre / scale numeric predictors (keeps t-SNE happy) ──────────
numeric_cols <- names(X)
X_scaled <- scale(X)

# ── 4.  run t-SNE (2-D) ────────────────────────────────────────────────
set.seed(123)
tsne_out <- Rtsne(
  X_scaled,
  dims        = 2,
  perplexity  = 50,     # good for 150k–200k points
  theta       = 0.5,    # Barnes–Hut speed-up
  verbose     = TRUE,
  pca         = TRUE,
  check_duplicates = FALSE
)

# ── 5.  wrap into a ggplot, colour by approval label ────────────────────
tsne_df <- data.frame(
  X      = tsne_out$Y[, 1],
  Y      = tsne_out$Y[, 2],
  group  = ifelse(temp$IS_APPROVED == 1, "Approved", "Denied")
)

ggplot(tsne_df, aes(X, Y, colour = group)) +
  geom_point(alpha = 0.3, size = 0.6) +
  scale_colour_manual(values = c("Approved" = "#1f78b4",
                                 "Denied"   = "#e31a1c")) +
  labs(title = "t-SNE of Maryland SNAP Positives vs. Synthetic Negatives",
       x = "t-SNE 1", y = "t-SNE 2", colour = "Group") +
  theme_minimal(base_size = 13)


tsne_df$reason <- temp$DENIAL_CODE[as.numeric(rownames(temp))]

ggplot(tsne_df, aes(X, Y, colour = reason)) +
  geom_point(alpha = 0.4, size = 0.7) +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "t-SNE by Denial Reason", x = "t-SNE 1", y = "t-SNE 2") +
  theme_minimal(base_size = 13)


#Umap testing 
library(umap)
umap_conf <- umap.defaults
umap_conf$n_neighbors <- 30   # neighborhood size
umap_conf$min_dist    <- 0.05  # spread / tightness

umap_out <- umap(X_scaled, config = umap_conf)

cols <- c("NA" = "grey50",
          "EXCESS_GROSS" = "red",
          "EXCESS_GROSS+EXCESS_NET" = "blue",
          "EXCESS_GROSS+EXCESS_NET+SHELTER_CAP" = "forestgreen")

plot(umap_out$layout,
     col = cols[train_df$DENIAL_CODE],
     pch = 19, cex = 0.6,
     xlab = "UMAP-1", ylab = "UMAP-2",
     main = "UMAP embedding by denial reason")
legend("topright", legend = names(cols), col = cols, pch = 19)


#KNN testing for the boundry definition and testing if the clusters are well seperated enough 



#Counterfactual generation 

library("counterfactuals")
library("iml")
library("randomForest")
library("mlr3")
library("mlr3learners")




#Improvements: 
#1. We need to corrently fit the model instead of traning it on a wide variety of systems 
#2. Counterfactual testing 



set.seed(20210816)

train_df$IS_APPROVED <- as.factor(train_df$IS_APPROVED)
rf = randomForest::randomForest(IS_APPROVED ~ ., data = train_df[,c("CERTHHSZ","FSDEPDED","FSMEDDED","FSSLTEXP" ,"FSEARN" ,  "FSUNEARN"   ,"FSDIS" ,"FSELDER", "HOMELESS_DED", "IS_APPROVED")][-1], importance=TRUE)

#Note we are testing the rf classifier here 



predictor = iml::Predictor$new(rf, type = "prob")
x_interest = train_df[,c("CERTHHSZ","FSDEPDED","FSMEDDED","FSSLTEXP" ,"FSEARN" ,"FSUNEARN","FSDIS" ,"FSELDER", "HOMELESS_DED")][-1]
predictor$predict(x_interest)


#Counterfactual generation 
MOC_classif = MOCClassif$new(predictor, fixed_features = c("FSEARN" ,"FSUNEARN"))

#Readding the dropped columns
colnames(data_cleaned_maryland)
keep_cols <- setdiff(names(data_cleaned_maryland),
                     names(counterfactual_data))

cfact<- 1999

cfactuals = MOC_classif$find_counterfactuals(
  x_interest[cfact], desired_class = '1', desired_prob = c(0.5, 1)
)

counterfactual_data <- cfactuals$data



#print(cfactuals)
#x_interest[cfact]





#Check if this is eligible after the counterfactual




#Final testing
x_interest[cfact]
print(cfactuals)

#Check if this is ineligible before the counterfactual
snap_md_eligibility(train_df[cfact])
snap_md_eligibility(cbind(train_df[ , ..keep_cols][cfact], counterfactual_data[1]))


#Run this into an easy LLM, connected to the digital ocean 

library(httr)
library(jsonlite)



response <- POST(
  url = "http://104.236.203.213:8000/llm",
  body = list(input = "You are a policy assistant helping users understand what changes they need to make to become eligible for SNAP benefits.

Here is a user's current profile (factual):
CERTHHSZ: 4
FSDEPDED: 3844
FSMEDDED: 0
FSSLTEXP: 2000
FSEARN: 4698
FSUNEARN: 3287.5
FSDIS: 0
FSELDER: 0
HOMELESS_DED: 0

Here is the profile needed to become eligible (counterfactual):
CERTHHSZ: 7
FSDEPDED: 310.7992
FSMEDDED: 0
FSSLTEXP: 2000
FSEARN: 4698
FSUNEARN: 3287.5
FSDIS: 0
FSELDER: 0
HOMELESS_DED: 0

Please explain in plain language what this person needs to change to become eligible. Focus only on the differences. Suggest ways to achieve this as well.
"),
  encode = "json"
)

cat(content(response)$response)













