library(tidyverse)
library(dplyr)
library(readxl)

data <- read_excel("data/mekanismeartikkel.xlsx", sheet = 1)

data %>% print(n=Inf)

data <- data %>% 
  clean_names()

df_main <- data %>%
  select(
    id,
    group,
    alder_pre,
    hoyde_pre,
    vekt_pre,
    bmi_pre,
    hoyde_post,
    vekt_post,
    bmi_post,
    
    q_l_min_pre = pre_q_lmin,
    q_l_min_post = post_q_lmin,
    
    hr_ekko_pre = pre_h_rekko,
    hr_ekko_post = post_hrekko,
    
    mvv_l_min_pre = pre_mvv_l_min,
    mvv_l_min_post = post_mvv_l_min,
    
    hb_pre = pre_mengde_hb_gram,
    hb_post = post_mengde_hb_gra,
    
    csa_total_pre,
    csa_total_post,
    caf_total_pre,
    caf_total_post,
    cs_pre,
    cs_post,
    hadh_pre,
    hadh_post
  )

df_full <- data %>%
  select(
    
    # deskriptive
    id,
    group,
    alder_pre,
    hoyde_pre,
    vekt_pre,
    bmi_pre,
    hoyde_post,
    vekt_post,
    bmi_post,
    
    # muskelvariabler
    csa_mhc1_pre,
    csa_mhc1_post,
    csa_mhc2_pre,
    csa_mhc2_post,
    csa_total_pre,
    csa_total_post,
    
    proportion_mhc1_pre,
    proportion_mhc1_post,
    proportion_mhc2_pre,
    proportion_mhc2_post,
    
    caf_mhc1_pre,
    caf_mhc1_post,
    caf_mhc2_pre,
    caf_mhc2_post,
    caf_total_pre,
    caf_total_post,
    
    cafa_mhc1_pre,
    cafa_mhc1_post,
    cafa_mhc2_pre,
    cafa_mhc2_post,
    cafa_total_pre,
    cafa_total_post,
    
    cs_pre,
    cs_post,
    hadh_pre,
    hadh_post,
    
    # kardiorespiratoriske
    vo2lmin_pre = pre_vo2lmin,
    vo2lmin_post = post_vo2lmin,
    
    vo2mlkgmin_pre = pre_vo2mlkgmin,
    vo2mlkgmin_post = post_vo2mlkgmin,
    
    laktat_pre = pre_laktat,
    laktat_post = post_laktat,
    
    hfmaks_pre = pre_h_fmaks,
    hfmaks_post = post_h_fmaks,
    
    o2puls_pre = pre_o2puls,
    o2puls_post = post_o2puls,
    
    borgs_pre = pre_borgs,
    borgs_post = post_borgs,
    
    rer_maks_pre = pre_re_rmaks,
    rer_maks_post = post_re_rmaks,
    
    ve_maks_pre = pre_v_emaks_l,
    ve_maks_post = post_v_emaks_l,
    
    # hjertevariabler stressekko
    hr_ekko_pre = pre_h_rekko,
    hr_ekko_post = post_hrekko,
    
    q_lmin_pre = pre_q_lmin,
    q_lmin_post = post_q_lmin,
    
    sv_m_l_pre = pre_sv_m_l,
    sv_m_l_post = post_sv_m_l,
    
    tidtil_q_pre = pre_tidtil_q,
    tidtil_q_post = post_tidtil_q,
    
    # lungefunksjon
    mvv_l_min_pre = pre_mvv_l_min,
    mvv_l_min_post = post_mvv_l_min,
    
    # blodvolum
    hb_pre = pre_mengde_hb_gram,
    hb_post = post_mengde_hb_gra,
    
    # kroppssammensetning
    legs_lean_pre = pre_legs_lean_mass_gram,
    legs_lean_post = post_legs_lean_mass_gram,
    
    tot_lean_pre = pre_total_lean_mass_kg,
    tot_lean_post = post_total_lean_mass_kg
  ) %>%
  mutate(
    legs_lean_kg_pre = legs_lean_pre / 1000,
    legs_lean_kg_post = legs_lean_post / 1000
  )


## lage change score variabler
create_change_scores <- function(df){
  
  post_vars <- grep("_post$", names(df), value = TRUE)
  
  for(v in post_vars){
    
    base <- sub("_post$", "", v)
    pre  <- paste0(base, "_pre")
    
    if(pre %in% names(df)){
      df[[paste0("delta_", base)]] <- df[[v]] - df[[pre]]
    }
  }
  
  df
}

df_full_delta <- create_change_scores(df_full)

# gjøre group til faktor 
df_full_v2 <- df_full_delta %>%
  mutate(
    group = factor(
      group,
      levels = c(0,1,2),
      labels = c(
        "Non-cancer controls",
        "BCS exercise",
        "BCS usual care"
      )
    )
  )


deskriptive <- c(
  "id","group",
  "alder_pre","hoyde_pre","vekt_pre","bmi_pre",
  "alder_post","hoyde_post","vekt_post","bmi_post"
)

muscle <- c(
  "csa_mhc1_pre","csa_mhc1_post",
  "csa_mhc2_pre","csa_mhc2_post",
  "csa_total_pre","csa_total_post",
  
  "proportion_mhc1_pre","proportion_mhc1_post",
  "proportion_mhc2_pre","proportion_mhc2_post",
  
  "caf_mhc1_pre","caf_mhc1_post",
  "caf_mhc2_pre","caf_mhc2_post",
  "caf_total_pre","caf_total_post",
  
  "cafa_mhc1_pre","cafa_mhc1_post",
  "cafa_mhc2_pre","cafa_mhc2_post",
  "cafa_total_pre","cafa_total_post",
  
  "cs_pre","cs_post",
  "hadh_pre","hadh_post"
)


cardio <- c(
  "vo2lmin_pre","vo2lmin_post",
  "vo2mlkgmin_pre","vo2mlkgmin_post",
  
  "laktat_pre","laktat_post",
  
  "hfmaks_pre","hfmaks_post",
  
  "o2puls_pre","o2puls_post",
  
  "borgs_pre","borgs_post",
  
  "rer_maks_pre","rer_maks_post",
  
  "ve_maks_pre","ve_maks_post"
)

heart <- c(
  "q_lmin_pre","q_lmin_post",
  
  "sv_m_l_pre","sv_m_l_post",
  
  "tidtil_q_pre","tidtil_q_post"
)

lung <- c(
  "mvv_l_min_pre","mvv_l_min_post"
)

blood <- c(
  "hb_pre","hb_post"
)

bodycomp <- c(
  "legs_lean_pre","legs_lean_post",
  "tot_lean_pre","tot_lean_post",
  
  "legs_lean_kg_pre","legs_lean_kg_post"
)


variable_groups <- list(
  deskriptive = deskriptive,
  muscle = muscle,
  cardio = cardio,
  heart = heart,
  lung = lung,
  blood = blood,
  bodycomp = bodycomp
)

