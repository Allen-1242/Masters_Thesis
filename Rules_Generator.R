
#Eligibility Rules calculation 

snap_md_eligibility <- function(df) {
  
  ## ── Replace missing numeric inputs with 0  -------------------------------
  vars_zero_if_na <- c("FSDEPDED", "FSMEDDED", "FSCSDED",
                       "FSSLTEXP", "FSEARN", "FSUNEARN", "FSBENSUPP")
  for (v in vars_zero_if_na) {
    if (v %in% names(df)) df[[v]][ is.na(df[[v]]) ] <- 0
  }
  #if ("FSBENSUPP" %in% names(df)) df$FSBENSUPP[ is.na(df$FSBENSUPP) ] <- 0
  
  
  ## ── Constants for FY-23‒25  ──────────────────────────────────────────
  FY_CONST <- list(
    "2023" = list(
      fpl100      = c(13590,18310,23030,27750,32470,37190)/12,
      std         = c(193,193,193,225,258,258),
      shelter_cap = 624,
      max_allot   = c(281,516,740,939,1116,1339)
    ),
    "2024" = list(
      fpl100      = c(14680,19860,25040,30220,35400,40580)/12,
      std         = c(198,198,198,208,244,279),
      shelter_cap = 672,
      max_allot   = c(291,535,766,973,1155,1386)
    ),
    "2025" = list(
      fpl100      = c(15650,21150,26650,32150,37650,43150)/12,
      std         = c(204,204,204,219,255,291),
      shelter_cap = 712,
      max_allot   = c(291,535,766,975,1155,1386)
    )
  )
  min_benefit <- 23                                    # 1–2-person floor
  
  ## ── Map YRMONTH → fiscal year  ------------------------------------------------
  fy_from_ym <- function(ym) {
    ym <- as.integer(ym); yr <- ym %/% 100; mo <- ym %% 100
    if (mo >= 10) yr <- yr + 1                        # Oct–Dec → next FY
    as.character(yr)
  }
  fy     <- fy_from_ym(df$YRMONTH[1])
  const  <- FY_CONST[[fy]]
  
  ## ── Pull constant vectors and name them  -------------------------------------
  fpl_100     <- const$fpl100;     names(fpl_100)     <- 1:6
  fpl_200     <- 2 * fpl_100
  std_table   <- const$std
  shelter_cap <- const$shelter_cap
  max_allot   <- const$max_allot; names(max_allot)    <- 1:6
  
  ## ── helper: standard deduction by size  --------------------------------------
  std_from_size <- function(size) {
    idx     <- pmin(size, 6)
    bracket <- cut(idx, breaks = c(0,3,4,5,Inf), labels = 1:4, right = TRUE)
    std_table[as.integer(bracket)]
  }
  
  ## ── 1. Income & deductions  --------------------------------------------------
  hhsize      <- pmin(df$CERTHHSZ, 6)
  gross       <- df$FSEARN + df$FSUNEARN
  earned_ded  <- floor(0.20 * df$FSEARN)
  std_ded_v   <- std_from_size(hhsize)
  adj_income  <- floor(gross - earned_ded - std_ded_v -
                         df$FSDEPDED - df$FSMEDDED)
  
  #Added the shelter cap
  HCSUA_FY23 <- 557          # Maryland, Oct 2022 – Sep 2023
  df$FSSLTEXP <- pmin(df$FSSLTEXP + HCSUA_FY23, 2000)  # add SUA, keep sane upper bound
  

  ## ── 2. Excess-shelter deduction  --------------------------------------------
  excess_shelter <- pmax(0, df$FSSLTEXP - floor(0.5 * adj_income))
  excess_shelter <- floor(pmin(excess_shelter, shelter_cap))
  net_income     <- adj_income - excess_shelter
  
  ## ── 3. Eligibility flags  ----------------------------------------------------
  pass_gross <- gross     <= fpl_200[as.character(hhsize)]
  pass_net   <- net_income <= fpl_100[as.character(hhsize)]
  eligible   <- pass_gross # BBCE: no asset test hence the gross is enough in maryland 
  
  ## ── 4. Benefit amount  -------------------------------------------------------
  thirty_pct <- floor(0.30 * net_income)
  prelim_ben <- max_allot[as.character(hhsize)] - thirty_pct
  benefit    <- ifelse(hhsize <= 2,
                       pmax(prelim_ben, min_benefit),
                       pmax(prelim_ben, 0))
  benefit    <- ceiling(benefit)
  benefit[!eligible] <- 0
  
  ## ── 5. Return original + derived columns  -----------------------------------
  cbind(df,
        gross, earned_ded, std_ded = std_ded_v,
        adj_income, excess_shelter, net_income,
        pass_gross, pass_net, eligible,
        benefit_calc = benefit)
}
