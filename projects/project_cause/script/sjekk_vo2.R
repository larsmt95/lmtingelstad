# ── Deskriptiv: hva er faktisk endringen i hver gruppe? ──────────────────────

df_full_v2 %>%
  filter(group %in% c("BCS exercise", "Non-cancer controls")) %>%
  group_by(group) %>%
  summarise(
    n         = sum(!is.na(delta_vo2mlkgmin)),
    mean_pre  = mean(vo2mlkgmin_pre,  na.rm = TRUE),
    mean_post = mean(vo2mlkgmin_post, na.rm = TRUE),
    mean_delta = mean(delta_vo2mlkgmin, na.rm = TRUE),
    sd_delta   = sd(delta_vo2mlkgmin,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# ── Modellen i detalj ─────────────────────────────────────────────────────────

model_vo2 <- lm(
  delta_vo2mlkgmin ~ group,
  data = df_full_v2 %>%
    filter(group %in% c("BCS exercise", "Non-cancer controls")) %>%
    droplevels()
)

summary(model_vo2)
confint(model_vo2)

# ── Sammenlign med hovedartikkelen ────────────────────────────────────────────
# Hovedartikkelen rapporterte mean difference -1.4 ml/kg/min (p=0.003)
# med n=70 og n=69 - vi har n=21 og n=19

# Kraft-beregning: hvor stor utvalgsstørrelse trenger vi for å detektere 
# en forskjell på 1.4 ml/kg/min med SD ~2.87?
power.t.test(
  delta = 1.4,          # forventet forskjell fra hovedartikkel
  sd    = 2.873,        # pooled SD fra vårt utvalg
  sig.level = 0.05,
  power = 0.80
)

# Hva er faktisk statistisk kraft med n=21 og n=19?
power.t.test(
  n     = 20,           # ca gjennomsnitt av 21 og 19
  delta = 1.4,
  sd    = 2.873,
  sig.level = 0.05
)
