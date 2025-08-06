# 3.Tiền xử lý dữ liệu
# 3.1 Ghép dữ liệu
# Load thư viện sử dụng và nối hai file CSV lại
required_packages <- c("this.path", "dplyr", "ggplot2", "lubridate", "geosphere", "readr", "corrplot", "faraway", "car", "ggthemes","gt","nortest","knitr","FSA","ggcorrplot","dunn.test","BSDA")
for (p in required_packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}
# Load đường dẫn hiện tại của thư mục data chứa các file CSV mẫu
setwd(this.path::here())
dirty_data <- read_csv("data/dirty_data.csv")
missing_data <- read_csv("data/missing_data.csv")
# Chuyển định dạng tháng ngày năm cột date
dirty_data$date <- parse_date_time(dirty_data$date, orders = c("mdy", "ymd", "dmy"))
missing_data$date <- parse_date_time(missing_data$date, orders = c("mdy", "ymd", "dmy"))
merged_data <- rbind(dirty_data, missing_data)
# ------------------------------------------------
# 3.2. Hiện thị giá trị bị khuyết sau khi ghép dữ liệu
na_cout<- colSums(is.na(merged_data ))
print(na_cout)

# ------------------------------------------------
# 3.3.1 Xử lý cột order_price và order_total
merged_data <- merged_data %>%
  mutate(
    order_total = ifelse(is.na(order_total), order_price * (100 - coupon_discount) / 100 + delivery_charges, order_total),
    order_price = ifelse(is.na(order_price), (order_total - delivery_charges) * 100 / (100 - coupon_discount), order_price)
)
# ------------------------------------------------
# 3.3.2 Cột season ban đầu
season_unique_before<-unique(merged_data$season)
print(season_unique_before)
# ------------------------------------------------
# 3.3.2 Làm sạch cột season
merged_data$season <- tolower(merged_data$season) # Đổi tất cả giá trị mùa dạng viết thường
month_value <- month(merged_data$date) # lấy tháng trong cột $date
merged_data <- merged_data %>%
  # Toán tử %>% : Truyền kết quả của phép toán hoặc hàm vào hàm tiếp theo .
  # mutate: Tạo ra cột mới hoặc thay đổi giá trị các cột trong data frame
  mutate(season = case_when(
    !is.na(season) ~ season,
    month_value %in% c(12, 1, 2) ~ "winter",
    month_value %in% c(3, 4, 5) ~ "spring",
    month_value %in% c(6, 7, 8) ~ "summer",
    TRUE ~ "autumn"
  ))
# ------------------------------------------------
# 3.3.2 Kiểm tra giá trị cột season sau khi xử lý
season_unique_after<-unique(merged_data$season)
print(season_unique_after)

# ------------------------------------------------
# 3.3.3 Xử lý cột is_happy_customer
median_happy_customer <- round(median(merged_data$is_happy_customer, na.rm = TRUE), digits = 0)
merged_data$is_happy_customer[is.na(merged_data$is_happy_customer)] <- median_happy_customer

# -----------------------------------------------
# 3.2.4 Sau khi làm sạch
na_cout<- colSums(is.na(merged_data ))
print(na_cout)

# ------------------------------------------------
# 4.1 Hiện thị các điểm ngoại lai (Outlier)
ggplot(data = merged_data, aes(y = order_price)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Điểm ngoại lai của order_price",
    y = ""
  )
ggplot(data = merged_data, aes(y = order_total)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Điểm ngoại lai của order_total",
    y = ""
  )
# ------------------------------------------------
# 4.1 Sử dụng công thức tứ phân vị (IQR)
# Tính toán các tứ phân vị cho các cột order_price và order_total
quantiles_price <- quantile(merged_data$order_price)
quantiles_total <- quantile(merged_data$order_total)

# Xác định Q1 và Q3 cho order_price và order_total
q1_price <- quantiles_price[2]
q3_price <- quantiles_price[4]
q1_total <- quantiles_total[2]
q3_total <- quantiles_total[4]

# Tính IQR cho order_price và order_total
IQR_price <- q3_price - q1_price
IQR_total <- q3_total - q1_total

# Hàm tính giá trị biên dưới (lower) và biên trên (upper) của IQR
calc_lower <- function(Q1, IQR) { return(Q1 - 1.5 * IQR) }
calc_upper <- function(Q3, IQR) { return(Q3 + 1.5 * IQR) }

# Tính giá trị biên dưới và biên trên cho order_price và order_total
lower_price <- calc_lower(q1_price, IQR_price)
upper_price <- calc_upper(q3_price, IQR_price)
lower_total <- calc_lower(q1_total, IQR_total)
upper_total <- calc_upper(q3_total, IQR_total)

# Điều chỉnh giá trị order_total ra ngoài phạm vi IQR
for (i in 1:length(merged_data$order_total)) {
  if (merged_data$order_total[i] > upper_total) {
    merged_data$order_total[i] = upper_total  # Giới hạn giá trị trên của order_total
  } else if(merged_data$order_total[i] < lower_total ){
    merged_data$order_total[i] = lower_total  # Giới hạn giá trị dưới của order_total
  }
}

# Điều chỉnh giá trị order_price ra ngoài phạm vi IQR
for (i in 1:length(merged_data$order_price)) {
  if (merged_data$order_price[i] > upper_price) {
    merged_data$order_price[i] = upper_price  # Giới hạn giá trị trên của order_price
  } else if(merged_data$order_price[i] < lower_price){
    merged_data$order_price[i] = lower_price  # Giới hạn giá trị dưới của order_price
  }
} 
# ------------------------------------------------------
# 4.1 Đồ thị thể hiện tứ phân vị của cột order_price
ggplot(data = merged_data, aes(y = order_price)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Đồ thị thể hiện tứ phân vị của cột Order Price",
  )
ggplot(data = merged_data, aes(y = order_total)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red", outlier.fill = "red") +
  theme_minimal() +
  labs(
    title = "Đồ thị thể hiện tứ phân vị của cột Order Total",
  )
# ------------------------------------------------------
# 4.2 Thống kê số liệu theo mùa và các hạng mục [2] và [3]
season_summary <- merged_data %>%
  group_by(season) %>%
  summarise(
    total_orders = n(),
    avg_order_total = mean(order_total, na.rm = TRUE),
    total_delivery_charges = sum(delivery_charges, na.rm = TRUE)
  )
print(season_summary)

# ------------------------------------------------------
# 4.2 Thống kê dữ liệu
# 4.2.1 Tương quan dữ liệu giữa cột order_price và order_total
overview_data <- merged_data[c("order_price", "delivery_charges", "coupon_discount","order_total", "is_expedited_delivery", "is_happy_customer", "shopping_cart")]
numeric_data <- overview_data[sapply(overview_data, is.numeric)]
cor_matrix <- cor(numeric_data)
# Hiện thi ma trận
corrplot(
  cor_matrix,
  method = "color",
  col= colorRampPalette(c("white","#202020", "#202040"))(10) ,
  type = "full",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "white",
  number.cex = 0.8,
  tl.cex = 0.8,
  diag = TRUE,
  cl.pos = "r"
)
# ------------------------------------------------------
# 4.2.2 Đồ thị thể tổng số đơn hàng theo mùa
ggplot(data = season_summary, aes(x = season, y = total_orders, fill = season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_orders), vjust = 2,color = "white", ) +
  theme_minimal() +
  labs(
    title = "Tổng số đơn hàng trong từng mùa",
    x = " ",
    y = " " # Để trống
  ) +
  scale_fill_brewer(palette = "Paired")
# ------------------------------------------------------
# 4.2.3 Đồ thị thể tổng chi phí đơn hàng theo mùa
ggplot(data = season_summary, aes(x = season, y = total_delivery_charges, fill = season)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_delivery_charges), vjust = 2,color = "white", ) +
  theme_minimal() +
  labs(
    title = "Tổng phí giao hàng từng mùa",
    x = " ",
    y = " " #Để trống
  ) +
  scale_fill_brewer(palette = "Set2")

# ------------------------------------------------------
# 5.1 Kiểm tra phân phối chuẩn bằng phương pháp Shapiro-Wilk

# Trích xuất các cột order_price và order_total từ merged_data
get_order_price <- merged_data$order_price
get_order_total <- merged_data$order_total

# Kiểm tra phân phối chuẩn với Shapiro-Wilk cho cột order_total
shapiro_order_total <- shapiro.test(get_order_total)
# Kiểm tra phân phối chuẩn với Shapiro-Wilk cho cột order_price
shapiro_order_price <- shapiro.test(get_order_price)

# In kết quả kiểm tra Shapiro-Wilk cho toàn bộ dữ liệu
print(shapiro_order_price)  # Kết quả kiểm tra cho order_price
print(shapiro_order_total)  # Kết quả kiểm tra cho order_total
# Kiểm tra phân phối chuẩn theo từng mùa
seasons <- c("spring", "summer", "fall", "winter")
# Tạo danh sách để lưu giá trị order_price theo từng mùa
order_price_by_season <- list()
# Lặp qua từng mùa, trích xuất dữ liệu và lưu vào danh sách
for (season in seasons) {
  # Trích xuất dữ liệu của từng mùa bằng subset()
  season_data <- subset(merged_data, season == season)  
  # Lưu giá trị order_price của từng mùa vào danh sách
  order_price_by_season[[season]] <- season_data$order_price
}
# Tạo bảng tóm tắt kết quả kiểm tra Shapiro-Wilk cho từng mùa
shapiro_summary <- data.frame(
  Season = names(order_price_by_season),  # Tên các mùa
  P_value = sapply(order_price_by_season, function(order_price) {
    # Tính p-value từ kiểm tra Shapiro-Wilk cho từng mùa
    shapiro.test(order_price)$p.value
  })
)
# Đánh giá phân phối chuẩn: nếu p-value > 0.05 thì phân phối chuẩn
shapiro_summary$Normal_Distribution <- ifelse(shapiro_summary$P_value > 0.05, 
                                              "True",  # Phân phối chuẩn
                                              "False") # Không phải phân phối chuẩn
# In bảng tóm tắt kết quả kiểm tra Shapiro-Wilk theo mùa
shapiro_summary %>%
  gt() %>%
  tab_header(title=md("#### Kết quả phân tích phân phối chuẩn từng mùa"))
# Phần 5.2 Các phương pháp thống kê
# 5.2.1 Kiểm định 1 mẫu 
# ------------------------------------------------------
# Dùng t-test
# Không tuân theo phân phối chuẩn, không có phương sai
# Lọc dữ liệu sau khi gộp lại và lọc theo mùa

spring_data <- merged_data %>%
  filter(season == "spring")
## Xuất ra trung bình mẫu, sd , muy của order_total
spring_stats <- spring_data %>%
  summarise(
    mean = mean(order_total),
    sd = sd(order_total),
    n = n()
  )

print(spring_stats)
# Thực hiện kiểm định trung bình 1 mẫu với giá trị kì vọng > 50
z.test(x = spring_data$order_total,
       mu = 50,
       sigma.x = sd(spring_data$order_total),
       alternative = "greater")

# Sigma : độ lệch chuẩn của 
# 5.2.2 Kiểm định 2 mẫu độc lập 
# ------------------------------------------------------
# 2 mẫu độc lập , không tuân theo phân phối chuẩn (công thức 4d)
# tìm độ lệch chuẩn, số lượng từng nhóm

group_stats <- merged_data %>%
  group_by(season) %>%
  summarise(
    mean = mean(order_total),
    sd = sd(order_total),
    n = n(),
    .groups = "drop"  # tránh cảnh báo nhóm
  ) %>%
  filter(season %in% c("spring", "summer"))

print(group_stats)

# Độ lệch chuẩn 2 mùa xuân và hè 
sd_spring <- group_stats$sd[group_stats$season == "spring"]
sd_summer <- group_stats$sd[group_stats$season == "summer"]

## Tổng giá trị đơn hàng trong 2 mùa
spring_data <- merged_data$order_total[merged_data$season == "spring"]
summer_data <- merged_data$order_total[merged_data$season == "summer"]
### T-test
t_test_result_two_sample<-t.test(order_total ~ season, 
       data = merged_data %>% filter(season %in% c("spring", "summer")),
       var.equal = FALSE) 
print(t_test_result_two_sample)

