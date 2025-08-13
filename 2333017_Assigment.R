##################################################
# 1. Load thư viện
##################################################
required_packages <- c(
  "this.path", "dplyr", "ggplot2", "lubridate", "geosphere",
  "readr", "corrplot", "faraway", "car", "ggthemes", "gt",
  "nortest", "knitr", "FSA", "ggcorrplot", "dunn.test",
  "BSDA", "writexl","webshot"
)
for (p in required_packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

##################################################
# 2. Tiền xử lý & Ghép dữ liệu
##################################################
setwd(this.path::here())

dirty_data   <- read_csv("data/dirty_data.csv")
missing_data <- read_csv("data/missing_data.csv")

dirty_data$date   <- parse_date_time(dirty_data$date, orders = c("mdy", "ymd", "dmy"))
missing_data$date <- parse_date_time(missing_data$date, orders = c("mdy", "ymd", "dmy"))

merged_data <- rbind(dirty_data, missing_data)

# NA ban đầu
print(colSums(is.na(merged_data)))

##################################################
# 3. Làm sạch dữ liệu
##################################################
# 3.1 Xử lý order_price & order_total
merged_data <- merged_data %>%
  mutate(
    order_total = ifelse(is.na(order_total),
                         order_price * (100 - coupon_discount) / 100 + delivery_charges,
                         order_total),
    order_price = ifelse(is.na(order_price),
                         (order_total - delivery_charges) * 100 / (100 - coupon_discount),
                         order_price)
  )

# 3.2 Làm sạch season
merged_data$season <- tolower(merged_data$season)
month_value <- month(merged_data$date)
merged_data <- merged_data %>%
  mutate(season = case_when(
    !is.na(season) ~ season,
    month_value %in% c(12, 1, 2) ~ "winter",
    month_value %in% c(3, 4, 5) ~ "spring",
    month_value %in% c(6, 7, 8) ~ "summer",
    TRUE ~ "autumn"
  ))

# 3.3 Điền NA cho is_happy_customer
median_happy <- round(median(merged_data$is_happy_customer, na.rm = TRUE), 0)
merged_data$is_happy_customer[is.na(merged_data$is_happy_customer)] <- median_happy

# NA sau khi xử lý
print(colSums(is.na(merged_data)))

write_xlsx(merged_data, "data/cleaned_merged_data.xlsx")

##################################################
# 4. Phát hiện ngoại lai trước xử lý
##################################################
# Boxplot order_price
ggplot(data = merged_data, aes(y = order_price)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(title = "Điểm ngoại lai của order_price", y = "")

# Boxplot order_total
ggplot(data = merged_data, aes(y = order_total)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(title = "Điểm ngoại lai của order_total", y = "")

##################################################
# 5. Xử lý ngoại lai (IQR)
##################################################
calc_lower <- function(Q1, IQR) Q1 - 1.5 * IQR
calc_upper <- function(Q3, IQR) Q3 + 1.5 * IQR

iqr_adjust <- function(vec) {
  Q1 <- quantile(vec, 0.25, na.rm = TRUE)
  Q3 <- quantile(vec, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- calc_lower(Q1, IQR_val)
  upper <- calc_upper(Q3, IQR_val)
  pmin(pmax(vec, lower), upper)
}

merged_data$order_total <- iqr_adjust(merged_data$order_total)
merged_data$order_price <- iqr_adjust(merged_data$order_price)

# Boxplot sau khi xử lý IQR
ggplot(data = merged_data, aes(y = order_price)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(title = "Đồ thị tứ phân vị của Order Price")

ggplot(data = merged_data, aes(y = order_total)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(title = "Đồ thị tứ phân vị của Order Total")

##################################################
# 6. Phân tích mô tả
##################################################
season_summary <- merged_data %>%
  group_by(season) %>%
  summarise(
    total_orders = n(),
    avg_order_total = mean(order_total, na.rm = TRUE),
    total_delivery_charges = sum(delivery_charges, na.rm = TRUE),
    .groups = "drop"
  )
print(season_summary)

# Corrplot
numeric_data <- merged_data %>%
  select(order_price, delivery_charges, coupon_discount,
         order_total, is_expedited_delivery, is_happy_customer, shopping_cart) %>%
  select(where(is.numeric))
cor_matrix <- cor(numeric_data)

corrplot(cor_matrix, method = "color",
         col = colorRampPalette(c("white","#202020", "#202040"))(10),
         type = "full", order = "hclust",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "white", number.cex = 0.8,
         tl.cex = 0.8, diag = TRUE, cl.pos = "r")

# Bar chart tổng số đơn hàng
ggplot(data = season_summary, aes(x = season, y = total_orders, fill = season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_orders), vjust = 2, color = "white") +
  theme_minimal() +
  labs(title = "Tổng số đơn hàng trong từng mùa", x = "", y = "") +
  scale_fill_brewer(palette = "Paired")

# Bar chart tổng phí giao hàng
ggplot(data = season_summary, aes(x = season, y = total_delivery_charges, fill = season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_delivery_charges), vjust = 2, color = "white") +
  theme_minimal() +
  labs(title = "Tổng phí giao hàng từng mùa", x = "", y = "") +
  scale_fill_brewer(palette = "Set2")

##################################################
# 7. Kiểm tra phân phối chuẩn (Shapiro-Wilk)
##################################################
shapiro_order_price <- shapiro.test(merged_data$order_price)
shapiro_order_total <- shapiro.test(merged_data$order_total)

print(shapiro_order_price)
print(shapiro_order_total)

##################################################
# 8. Kiểm định thống kê
##################################################
## 8.1 Kiểm định 1 mẫu
spring_data <- merged_data %>% filter(season == "spring")
spring_stats <- spring_data %>% summarise(
  mean = mean(order_total),
  sd = sd(order_total),
  n = n()
)
print(spring_stats)

z.test(x = spring_data$order_total,
       mu = 50,
       sigma.x = sd(spring_data$order_total),
       alternative = "greater")

## 8.2 Kiểm định 2 mẫu độc lập
group_stats <- merged_data %>%
  filter(season %in% c("spring", "summer")) %>%
  group_by(season) %>%
  summarise(
    mean = mean(order_total),
    sd = sd(order_total),
    n = n(),
    .groups = "drop"
  )
print(group_stats)

t_test_result <- t.test(order_total ~ season,
                        data = merged_data %>% filter(season %in% c("spring", "summer")),
                        var.equal = FALSE)
print(t_test_result)

##################################################
# 9. ANOVA (4 mùa)
##################################################
# dùng phương pháp leveneTest trong R
df_cleaned <- merged_data %>%
  group_by(season) %>%
  mutate(order_total_cleaned = ifelse(order_total > quantile(order_total, 0.75) + 1.5 * IQR(order_total) |
                                      order_total < quantile(order_total, 0.25) - 1.5 * IQR(order_total),
                                      NA, order_total)) %>%
  ungroup()

leveneTest(order_total_cleaned ~ season, data = df_cleaned)
anova_model <- aov(order_total_cleaned ~ season, data = df_cleaned)
summary(anova_model)

# Boxplot ANOVA
ggplot(df_cleaned, aes(x = season, y = order_total_cleaned)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "So sánh Order Total giữa các mùa (ANOVA)")


# Mở rộng: Dunn test
# Krustal_wallis
kruskal_result <- kruskal.test(order_price ~ season, data = merged_data)
print(kruskal_result)