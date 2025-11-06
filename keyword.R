# Olam, keyword analysis
setwd("~/Desktop/jobs/olam-proj")
library(stringi)
library(fmsb)

recode_as_factor <- function(x, recode_ls, ordered = FALSE) {
  x_recoded <- recode(x, !!!recode_ls)
  if (ordered) {
    factor(x_recoded, levels = recode_ls)
  } else {
    factor(x_recoded)
  }
}
# 1. import ------------------------------------------------------------
dir_path <- "./policy"
dir_output <- "./supplement"
files_ls <- list.files(dir_path, full.names = TRUE)
files_eng_ls <- list.files(paste0(dir_path, "/eng"), full.names = TRUE)

dt_pdf <- data.table(file = files_ls) %>%
  mutate(node = str_extract(file, "(?<=\\[)\\d+(?=\\])") %>% as.integer()) %>%
  mutate(year = str_extract(file, "(?<=@)\\d+(?=.pdf)") %>% as.integer()) %>%
  mutate(name_chn = str_extract(file, "(?<=\\] - ).*(?=@)")) %>%
  dplyr::filter(!is.na(node)) %>%
  arrange(node) %>%
  cbind(agency = c(
    ## manually input
    rep("SC", 12), rep("MOA", 5), rep("CSRC", 7), "CFFEX", rep("ZCE", 4)
  ))

stageline_cutoff <- c(1992, 2000, 2006, 2010)
agency_cat <- c("SC" = "SC", "MOA" = "MOA", "CSRC" = "EX", "CFFEX" = "EX", "ZCE" = "EX")
color_polar <- c("#698714", "#ff7000")

## 1.1 markdown files ------------------------------------------------------------
read_one <- function(file) paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

dt_md <- data.table(file = files_eng_ls) %>%
  mutate(
    node = str_extract(file, "\\d+") %>% as.integer()
  ) %>%
  arrange(node) %>%
  left_join(dt_pdf[, .(node, year, agency, name_chn)], by = "node") %>%
  mutate(
    agency_cat = agency_cat[agency],
    stage = findInterval(year, stageline_cutoff, left.open = TRUE) + 1,
    text = sapply(file, read_one) %>% str_remove_all("[\r\n]"),
    text_chn = sapply(str_replace(file, "/eng/", "/chn/"), read_one) %>%
      str_remove_all("[\r\n ]")
  )

# 2. keyword analysis ------------------------------------------------------------
kw_ls_ch <- list(
  risk_all = c("预期", "风险", "保值", "套期"),
  risk_price_all = c("价格"),
  fluctuation = c("波动", "变动", "稳定"),
  agri_all = c("农", "粮"),
  future = c("期货", "期权", "跨期", "远期"), # 交易
  agri_future = c("农产品期货"),
  # specific interest
  income = c("农民收入", "收入波动", "收入稳定", "稳定收入", "收入增长", "收入"),
  risk_general = c("风险管理", "管理风险", "控制风险", "风险控制", "预期管理", "管理预期", "跨期保值", "套期保值"),
  risk_price = c("价格风险", "价格波动", "价格变动", "价格稳定", "稳定价格"),
  risk_pricing = c("价格发现", "价格形成", "参考价格"),
  dev_prod = c("农产品流通", "农产品生产", "农业效率", "粮食产量", "粮食安全", "粮食生产"),
  dev_argi = c("农村经济", "农业经济", "三农", "农业现代化", "农业规范化", "农业发展", "农产品期货")
)

dt_kwa_chn <- sapply(kw_ls_ch, function(kw_cat) {
  stri_count_regex(dt_md$text_chn, paste(kw_cat, collapse = "|"))
}) |>
  as.data.table() |>
  cbind(dt_md[, .(node, year, stage, agency, agency_cat)], kw = _)

## 2.1. plot ----------------------------------------------------------------
## correlation plot
fig_corr <- dt_kwa_chn |>
  select(kw.risk_all:kw.income) |>
  cor() |>
  as.data.frame() |>
  tibble::rownames_to_column("var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "correlation") |>
  mutate(across(
    c(var1, var2),
    ~ recode_as_factor(
      .x,
      recode_ls = c(
        kw.risk_all       = "Risk",
        kw.risk_price_all = "Price",
        kw.fluctuation    = "Fluctuation",
        kw.agri_all       = "Agri.",
        kw.future         = "Futures",
        kw.agri_future    = "Agri. futures",
        kw.income         = "Farmer income"
      ),
      ordered = TRUE
    )
  )) |>
  dplyr::filter(as.numeric(var1) <= as.numeric(as.factor(var2))) |>
  ggplot(aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    low = color_polar[2],
    mid = "white",
    high = color_polar[1],
    midpoint = 0,
    name = "Correlation"
  ) +
  geom_text(aes(label = round(correlation, 2)), size = 3) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "Keyword Correlation Matrix")
## area plot
fig_area <- dt_kwa_chn |>
  select(year, kw.risk_all:kw.income) |>
  group_by(year) |>
  summarise(across(starts_with("kw."), sum), .groups = "drop") |>
  pivot_longer(starts_with("kw."), names_to = "keyword", values_to = "count") |>
  mutate(
    keyword = recode_as_factor(
      keyword,
      recode_ls = c(
        kw.agri_all       = "Agri.",
        kw.agri_future    = "Agri. futures",
        kw.fluctuation    = "Fluctuation",
        kw.future         = "Futures",
        kw.income         = "Farmer income",
        kw.risk_all       = "Risk",
        kw.risk_price_all = "Price"
      ),
      ordered = TRUE
    )
  ) |>
  ggplot(aes(x = year, y = count, fill = keyword)) +
  geom_area(position = "stack", alpha = 0.7) +
  scale_fill_viridis_d(name = "Keyword") +
  labs(
    title = "Keyword Composition Over Time",
    x = "Year", y = "Cumulative Frequency"
  ) +
  theme_minimal()
## bar plot, specific
fig_kwa_chn_spec <- dt_kwa_chn %>%
  select(
    node, year, stage, agency, agency_cat,
    kw.income:kw.dev_argi
  ) %>%
  summarise(
    .by = c(agency_cat, stage),
    across(starts_with("kw."), sum)
  ) |>
  pivot_longer(
    starts_with("kw."),
    names_to = "keyword", values_to = "n"
  ) |>
  mutate(
    keyword = recode_as_factor(
      keyword,
      recode_ls = c(
        kw.risk_general = "Risk mng.",
        kw.risk_price   = "Risk, price",
        kw.risk_pricing = "Risk, pricing",
        kw.income       = "Farmer income",
        kw.dev_prod     = "Dev., agri-prod.",
        kw.dev_argi     = "Dev., agri-econ."
      ),
      ordered = TRUE
    )
  ) |>
  ggplot(data = _, aes(x = factor(stage), y = n, fill = agency_cat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~keyword, scales = "free") +
  scale_fill_manual(
    values = color_scale(n = 3, dark = color_polar[1], light = color_polar[2])
  ) +
  labs(
    x = "Stage",
    y = "Frequency",
    fill = "Agency",
    title = "Keyword frequency in policy documents (Chinese)",
    subtitle = "Specific interest keywords"
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    labels = scales::label_number(accuracy = 1)
  ) +
  theme_minimal() +
  theme(
    strip.text   = element_text(size = 12),
    axis.text.x  = element_text(size = 12, hjust = 1),
    axis.text.y  = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    plot.margin  = margin(t = 5, r = 5, b = 21, l = 5)
  )
## bar plot, general
fig_kwa_chn_genrl <- dt_kwa_chn %>%
  select(
    node, year, stage, agency, agency_cat,
    kw.risk_all:kw.agri_future
  ) %>%
  summarise(
    .by = c(agency_cat, stage),
    across(starts_with("kw."), sum)
  ) |>
  pivot_longer(
    starts_with("kw."),
    names_to = "keyword", values_to = "n"
  ) |>
  mutate(
    keyword = recode_as_factor(
      keyword,
      recode_ls = c(
        kw.risk_all       = "Risk",
        kw.risk_price_all = "Price",
        kw.fluctuation    = "Fluctuation",
        kw.agri_all       = "Agri.",
        kw.future         = "Futures",
        kw.agri_future    = "Agri. futures"
      ),
      ordered = TRUE
    )
  ) |>
  ggplot(data = _, aes(x = factor(stage), y = n, fill = agency_cat)) +
  geom_bar(stat = "identity") +
  facet_wrap(~keyword, scales = "free") +
  scale_fill_manual(
    values = color_scale(n = 3, dark = color_polar[1], light = color_polar[2])
  ) +
  labs(
    x = "Stage",
    y = "Frequency",
    fill = "Agency",
    title = "Keyword frequency in policy documents (Chinese)",
    subtitle = "General keywords"
  ) +
  theme_minimal() +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    labels = scales::label_number(accuracy = 1)
  ) +
  theme(
    strip.text   = element_text(size = 12),
    axis.text.x  = element_text(size = 12, hjust = 1),
    axis.text.y  = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    plot.margin  = margin(t = 5, r = 5, b = 21, l = 5)
  )

## I/O
ggsave(
  filename = paste0(dir_output, "/fig_kwa_chn_spec", ".pdf"),
  plot = fig_kwa_chn_spec,
  width = 10, height = 7
)
ggsave(
  filename = paste0(dir_output, "/fig_kwa_chn_genrl", ".pdf"),
  plot = fig_kwa_chn_genrl,
  width = 10, height = 7
)
ggsave(
  filename = paste0(dir_output, "/fig_corr", ".pdf"),
  plot = fig_corr,
  width = 6, height = 5
)
ggsave(
  filename = paste0(dir_output, "/fig_area", ".pdf"),
  plot = fig_area,
  width = 8, height = 6
)
fwrite(dt_kwa_chn, file = paste0(dir_output, "/dt_kwa_chn.csv"))
fwrite(dt_md,      file = paste0(dir_output, "/dt_md.csv"))

## 2.2. radar plot ------------------------------------------------------------
## 数据准备：按 agency_cat 汇总并标准化
dt_radar <- dt_kwa_chn |>
  mutate(
    kws_price  = kw.risk_price + kw.risk_pricing,
    kws_risk   = kw.risk_general + kw.fluctuation,
    kws_income = kw.income,
    kws_dev    = kw.dev_prod + kw.dev_argi,
    kws_agri   = kw.agri_all,
  ) |>
  select(agency_cat, starts_with("kws_")) |>
  group_by(agency_cat) |>
  summarise(across(everything(), sum), .groups = "drop")

## fmsb 需要数据框第一行为最大值，第二行为最小值
radar_data <- as.data.frame(dt_radar[,-1])
rownames(radar_data) <- dt_radar$agency_cat
radar_data <- rbind(apply(radar_data, 2, max),
                    apply(radar_data, 2, min),
                    radar_data)
radar_data %<>% rename(
  "Price" = "kws_price",
  "Risk" = "kws_risk",
  "Income" = "kws_income",
  "Development" = "kws_dev",
  "Agriculture" = "kws_agri"
)

## base R plot
pdf(paste0(dir_output, "/fig_radar_chart.pdf"), width = 8, height = 6)
opar <- par(mar = c(0.5, 0.5, 2, 0.5))
radarchart(radar_data,
           axistype = 1,
           pcol = c("#698714", "#ff7000", "#1f78b4", "#e31a1c", "#33a02c"),
           pfcol = scales::alpha(c("#698714", "#ff7000", "#1f78b4", "#e31a1c", "#33a02c"), 0.3),
           plwd = 2,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           vlcex = 1.2,
           vlabels = colnames(radar_data),
           title = "Agency Keyword Profile Radar Chart"
)
legend("topright", legend = rownames(radar_data)[-c(1,2)],
       col = c("#698714", "#ff7000", "#1f78b4", "#e31a1c", "#33a02c"), lty = 1, lwd = 2, bty = "n")
par(opar)
dev.off()

# X. fuzzy matching ------------------------------------------------------------
build_AB_regex_cn_chars <- function(A, B, min_chars = 0, max_chars = Inf,
                                    allow_newline = TRUE,
                                    ignore_case = FALSE, literal = TRUE) {
  esc <- function(x) if (literal) stringi::stri_escape_regex(x) else x
  A_alt <- paste(esc(A), collapse = "|")
  B_alt <- paste(esc(B), collapse = "|")

  dot <- if (allow_newline) "(?s)." else "."
  upper <- if (is.finite(max_chars)) max_chars else ""
  gap <- paste0(dot, "{", min_chars, ",", upper, "}?")

  pat <- paste0("(?:", A_alt, ")", gap, "(?:", B_alt, ")")
  if (ignore_case) paste0("(?i)", pat) else pat
}
