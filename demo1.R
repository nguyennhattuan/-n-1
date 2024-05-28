


library(TTR)

library(rvest)

library(tidyverse)

library(dplyr)

library(plotly)

library(ggplot2)




### Lấy và làm sạch dữ liệu
##Phần lấy dữu liệu
#nguon
URL <-
  "https://www.cophieu68.vn/category/category_summary.php?id=^nh"
URL1 <- "https://www.cophieu68.vn/quote/history.php?id=vcb"
URL2 <- "https://www.cophieu68.vn/quote/history.php?id=cts"
URL3 <- "https://www.cophieu68.vn/quote/history.php?id=tcb"
URL4 <- "https://www.cophieu68.vn/quote/history.php?id=msb"
URL5 <- "https://www.cophieu68.vn/quote/history.php?id=vpb"
URL6 <- "https://www.cophieu68.vn/quote/history.php?id=bid"

#doc file
html <- read_html(URL)
html1 <- read_html(URL1)
html2 <- read_html(URL2)
html3 <- read_html(URL3)
html4 <- read_html(URL4)
html5 <- read_html(URL5)
html6 <- read_html(URL6)

ma_CK <- html %>%
  html_nodes(".td_sticky_left , .td_sticky_left div") %>%
  html_text() %>%
  trimws()

von <- html %>%
  html_nodes(".tr_header td:nth-child(3) , .stock_online td:nth-child(3) , .tbody:nth-child(3)")  %>%
  html_text() %>%
  trimws()

anh_huong <- html %>%
  html_nodes("td:nth-child(4)") %>%
  html_text() %>%
  trimws()
#phan_tich_du_lieu_thanh_dang_bang
ma_CK_unique <- unique(ma_CK[ma_CK != ""])
# Tạo data frame từ ma_CK đã làm sạch và các vectơ dữ liệu khác
data <- data.frame(
  Ma_CK = ma_CK_unique,
  Von = von,
  Anh_Huong = anh_huong
)
# Hàm tính đường trung bình động (SMA) cho một mã chứng khoán cụ thể
tinh_sma <- function(df, ma_ck, period) {
  # Lọc dữ liệu cho mã CK cụ thể
  ma_ck_data <- df %>% filter(`Ma_CK` == ma_ck)
  
  # Chuyển cột Ngày thành dạng ngày tháng
  ma_ck_data$Ngay <- as.Date(ma_ck_data$Ngay, format = "%d/%m/%Y")
  
  # Chuyển cột đóng cửa thành số
  ma_ck_data$`dong_cua` <- as.numeric(gsub(",", "", ma_ck_data$`dong_cua`))
  
  # Tính đường trung bình động (SMA)
  sma <- SMA(ma_ck_data$`dong_cua`, n = period)
  
  # Tạo DataFrame mới chứa kết quả SMA
  sma_df <- data.frame("Ngay" = ma_ck_data$Ngay, "SMA" = sma)
  
  return(sma_df)
}
#ham tinh macd
tinh_macd <- function(df, ma_ck, fast_period = 12, slow_period = 26, signal_period = 9) {
  # Lọc dữ liệu cho mã CK cụ thể
  ma_ck_data <- df %>% filter(`Ma_CK` == ma_ck)
  
  # Chuyển cột Ngay thành dạng ngày tháng
  ma_ck_data$Ngay <- as.Date(ma_ck_data$Ngay, format = "%d/%m/%Y")
  
  # Chuyển cột đóng cửa thành số
  ma_ck_data$`dong_cua` <- as.numeric(gsub(",", "", ma_ck_data$`dong_cua`))
  
  # Tính Exponential Moving Average (EMA) nhanh và chậm
  ema_fast <- EMA(ma_ck_data$`dong_cua`, n = fast_period)
  ema_slow <- EMA(ma_ck_data$`dong_cua`, n = slow_period)
  
  # Tính MACD Line
  macd_line <- ema_fast - ema_slow
  
  # Tính Signal Line (EMA của MACD Line)
  signal_line <- EMA(macd_line, n = signal_period)
  
  # Tính Histogram
  histogram <- macd_line - signal_line
  
  # Tạo DataFrame mới chứa kết quả MACD
  macd_df <- data.frame("Ngay" = ma_ck_data$Ngay, "MACD_Line" = macd_line, "Signal_Line" = signal_line, "Histogram" = histogram)
  
  return(macd_df)
}
# Hiển thị bảng
print(data <- data[-1, ])

##phần làm sạch dữ liệu
#1
vcb <- html1 %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table()
tables <- html_nodes(html1, "table")
colnames(vcb)[1] <- "Ma_CK"
vcb<-vcb %>%
  mutate(`Ma_CK` = "vcb") 
# Làm sạch cột 3 
column_3 <- tables[[2]] %>%
  html_nodes("td:nth-child(3)") %>%
  html_text()

# Tiến hành phân tích và làm sạch dữ liệu nếu cần
cleaned_column_3 <- gsub("[\r\n\t]", " ", column_3)
vcb <- vcb %>%
  mutate(Gia_Khop = cleaned_column_3)
vcb <- subset(vcb, select = -X3)
vcb_df <- data.frame(vcb)
vcb_df <- vcb_df[-1, ] %>%
  rename(
    `Ma_CK` = "Ma_CK",
    `Ngay` = "X2",
    `gia_khop` = "Gia_Khop",
    `khoi_luong` = 'X4',
    `mo_cua` = "X5",
    `cao_nhat` = "X6",
    `thap_nhat` = "X7",
    `NN_mua` = "X8",
    `NN_ban` = "X9",
    `Gia_Tri_NN(ti VND)` = "X10"
  ) %>%
  mutate(`dong_cua` = lag(`mo_cua`)) 
vcb_df[1,'dong_cua'] <- vcb_df[1,'mo_cua']
#Tinh sma va macd
sma_vcb20 <- tinh_sma(vcb_df, 'vcb', period = 20)
sma_vcb10 <- tinh_sma(vcb_df, 'vcb', period = 10)
macd_vcb <- tinh_macd(vcb_df,'vcb')
# Thêm cột MACD_Line vaf 
vcb_df <- bind_cols(vcb_df, macd_vcb["MACD_Line"],macd_vcb["Signal_Line"])
vcb_df <- bind_cols(vcb_df, sma_vcb20["SMA"],sma_vcb10["SMA"])
print(vcb_df)
#2
cts <- html2 %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table()
tables <- html_nodes(html2, "table")
colnames(cts)[1] <- "Ma_CK"
cts<-cts %>%
  mutate(`Ma_CK` = "cts") 
# Làm sạch cột 3 
column_3 <- tables[[2]] %>%
  html_nodes("td:nth-child(3)") %>%
  html_text()

# Tiến hành phân tích và làm sạch dữ liệu nếu cần
cleaned_column_3 <- gsub("[\r\n\t]", " ", column_3)
cts <- cts %>%
  mutate(Gia_Khop = cleaned_column_3)
cts <- subset(cts, select = -X3)
cts_df <- data.frame(cts)
cts_df <- cts_df[-1, ] %>%
  rename(
    `Ma_CK` = "Ma_CK",
    `Ngay` = "X2",
    `gia_khop` = "Gia_Khop",
    `khoi_luong` = 'X4',
    `mo_cua` = "X5",
    `cao_nhat` = "X6",
    `thap_nhat` = "X7",
    `NN_mua` = "X8",
    `NN_ban` = "X9",
    `Gia_Tri_NN(ti VND)` = "X10"
  ) %>%
  mutate(`dong_cua` = lag(`mo_cua`)) 
cts_df[1,'dong_cua'] <- cts_df[1,'mo_cua']
#Tinh sma va macd
sma_cts20 <- tinh_sma(cts_df, 'cts', period = 20)
sma_cts10 <- tinh_sma(cts_df, 'cts', period = 10)
macd_cts <- tinh_macd(cts_df,'cts')
# Thêm cột MACD_Line 
cts_df <- bind_cols(cts_df, macd_cts["MACD_Line"],macd_cts["Signal_Line"])
cts_df <- bind_cols(cts_df, sma_cts20["SMA"], sma_cts10["SMA"])

print(cts_df)   

#3
tcb <- html3 %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table()
tables <- html_nodes(html3, "table")
colnames(tcb)[1] <- "Ma_CK"
tcb<-tcb %>%
  mutate(`Ma_CK` = "tcb") 
# Làm sạch cột 3 
column_3 <- tables[[2]] %>%
  html_nodes("td:nth-child(3)") %>%
  html_text()

# Tiến hành phân tích và làm sạch dữ liệu nếu cần
cleaned_column_3 <- gsub("[\r\n\t]", " ", column_3)
tcb <- tcb %>%
  mutate(Gia_Khop = cleaned_column_3)
tcb <- subset(tcb, select = -X3)
tcb_df <- data.frame(tcb)
tcb_df <- tcb_df[-1, ] %>%
  rename(
    `Ma_CK` = "Ma_CK",
    `Ngay` = "X2",
    `gia_khop` = "Gia_Khop",
    `khoi_luong` = 'X4',
    `mo_cua` = "X5",
    `cao_nhat` = "X6",
    `thap_nhat` = "X7",
    `NN_mua` = "X8",
    `NN_ban` = "X9",
    `Gia_Tri_NN(ti VND)` = "X10"
  ) %>%
  mutate(`dong_cua` = lag(`mo_cua`)) 
tcb_df[1,'dong_cua'] <- tcb_df[1,'mo_cua']
#Tinh sma va macd
sma_tcb20 <- tinh_sma(tcb_df, 'tcb', period = 20)
sma_tcb10 <- tinh_sma(tcb_df, 'tcb', period = 10)
macd_tcb <- tinh_macd(tcb_df,'tcb')
# Thêm cột MACD_Line 
tcb_df <- bind_cols(tcb_df, macd_tcb["MACD_Line"],macd_tcb["Signal_Line"])
tcb_df <- bind_cols(tcb_df, sma_tcb20["SMA"], sma_tcb10["SMA"])

print(tcb_df)  

#4
msb <- html4 %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table()
tables <- html_nodes(html4, "table")
colnames(msb)[1] <- "Ma_CK"
msb<-msb %>%
  mutate(`Ma_CK` = "msb") 
# Làm sạch cột 3 
column_3 <- tables[[2]] %>%
  html_nodes("td:nth-child(3)") %>%
  html_text()

# Tiến hành phân tích và làm sạch dữ liệu nếu cần
cleaned_column_3 <- gsub("[\r\n\t]", " ", column_3)
msb <- msb %>%
  mutate(Gia_Khop = cleaned_column_3)
msb <- subset(msb, select = -X3)
msb_df <- data.frame(msb)
msb_df <- msb_df[-1, ] %>%
  rename(
    `Ma_CK` = "Ma_CK",
    `Ngay` = "X2",
    `gia_khop` = "Gia_Khop",
    `khoi_luong` = 'X4',
    `mo_cua` = "X5",
    `cao_nhat` = "X6",
    `thap_nhat` = "X7",
    `NN_mua` = "X8",
    `NN_ban` = "X9",
    `Gia_Tri_NN(ti VND)` = "X10"
  ) %>%
  mutate(`dong_cua` = lag(`mo_cua`)) 
msb_df[1,'dong_cua'] <- msb_df[1,'mo_cua']
#Tinh sma va macd
sma_msb20 <- tinh_sma(msb_df, 'msb', period = 20)
sma_msb10 <- tinh_sma(msb_df, 'msb', period = 10)
macd_msb <- tinh_macd(msb_df,'msb')
# Thêm cột MACD_Line 
msb_df <- bind_cols(msb_df, macd_msb["MACD_Line"],macd_msb["Signal_Line"])
msb_df <- bind_cols(msb_df, sma_msb20["SMA"], sma_msb10["SMA"])
print(msb_df) 

#5
vpb <- html5 %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table()
tables <- html_nodes(html5, "table")
colnames(vpb)[1] <- "Ma_CK"
vpb<-vpb %>%
  mutate(`Ma_CK` = "vpb") 
# Làm sạch cột 3 
column_3 <- tables[[2]] %>%
  html_nodes("td:nth-child(3)") %>%
  html_text()

# Tiến hành phân tích và làm sạch dữ liệu nếu cần
cleaned_column_3 <- gsub("[\r\n\t]", " ", column_3)
vpb <-vpb %>%
  mutate(Gia_Khop = cleaned_column_3)
vpb <- subset(vpb, select = -X3)
vpb_df <- data.frame(vpb)
vpb_df <- vpb_df[-1, ] %>%
  rename(
    `Ma_CK` = "Ma_CK",
    `Ngay` = "X2",
    `gia_khop` = "Gia_Khop",
    `khoi_luong` = 'X4',
    `mo_cua` = "X5",
    `cao_nhat` = "X6",
    `thap_nhat` = "X7",
    `NN_mua` = "X8",
    `NN_ban` = "X9",
    `Gia_Tri_NN(ti VND)` = "X10"
  ) %>%
  mutate(`dong_cua` = lag(`mo_cua`)) 
vpb_df[1,'dong_cua'] <- vpb_df[1,'mo_cua']
#Tinh sma va macd
sma_vpb20 <- tinh_sma(vpb_df, 'vpb', period = 20)
sma_vpb10 <- tinh_sma(vpb_df, 'vpb', period = 10)
macd_vpb <- tinh_macd(vpb_df,'vpb')
# Thêm cột MACD_Line 
vpb_df <- bind_cols(vpb_df, macd_vpb["MACD_Line"],macd_vpb["Signal_Line"])
vpb_df <- bind_cols(vpb_df, sma_vpb20["SMA"], sma_vpb10["SMA"])

print(vpb_df)   

#6
bid <- html6 %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table()
tables <- html_nodes(html6, "table")
colnames(bid)[1] <- "Ma_CK"
bid<-bid %>%
  mutate(`Ma_CK` = "bid") 
# Làm sạch cột 3 
column_3 <- tables[[2]] %>%
  html_nodes("td:nth-child(3)") %>%
  html_text()

# Tiến hành phân tích và làm sạch dữ liệu nếu cần
cleaned_column_3 <- gsub("[\r\n\t]", " ", column_3)
bid <- bid %>%
  mutate(Gia_Khop = cleaned_column_3)
bid <- subset(bid, select = -X3)
bid_df <- data.frame(bid)
bid_df <- bid_df[-1, ] %>%
  rename(
    `Ma_CK` = "Ma_CK",
    `Ngay` = "X2",
    `gia_khop` = "Gia_Khop",
    `khoi_luong` = 'X4',
    `mo_cua` = "X5",
    `cao_nhat` = "X6",
    `thap_nhat` = "X7",
    `NN_mua` = "X8",
    `NN_ban` = "X9",
    `Gia_Tri_NN(ti VND)` = "X10"
  ) %>%
  mutate(`dong_cua` = lag(`mo_cua`)) 
bid_df[1,'dong_cua'] <- bid_df[1,'mo_cua']
#Tinh sma va macd
sma_bid20 <- tinh_sma(bid_df, 'bid', period = 20)
sma_bid10 <- tinh_sma(bid_df, 'bid', period = 10)
macd_bid <- tinh_macd(bid_df,'bid')
# Thêm cột MACD_Line 
bid_df <- bind_cols(bid_df, macd_bid["MACD_Line"],macd_bid["Signal_Line"])
bid_df <- bind_cols(bid_df, sma_bid20["SMA"],sma_bid10["SMA"])

print(bid_df) 


## Ghép các df thành 1 df tổng
df_tong <- bind_rows( vcb_df, cts_df, tcb_df, msb_df, vpb_df, bid_df)

# Hiển thị DataFrame tổng
print(df_tong)



### biểu diễn các đặc trưng thống kê
##tính trung bình
tinh_trung_binh <- function(df, ma_ck) {
  # Lọc dữ liệu cho mã CK cụ thể
  ma_ck_data <- df %>% filter(`Ma_CK` == ma_ck)
  
  # Loại bỏ dấu phẩy và chuyển đổi các cột thành số
  ma_ck_data$`khoi_luong` <- as.numeric(gsub(",", "", ma_ck_data$`khoi_luong`))
  ma_ck_data$`NN_mua` <- as.numeric(gsub(",", "", ma_ck_data$`NN_mua`))
  ma_ck_data$`NN_ban` <- as.numeric(gsub(",", "", ma_ck_data$`NN_ban`))
  ma_ck_data$`mo_cua` <- as.numeric(gsub(",", "", ma_ck_data$`mo_cua`))
  ma_ck_data$`dong_cua` <- as.numeric(gsub(",", "", ma_ck_data$`dong_cua`))
  ma_ck_data$`cao_nhat` <- as.numeric(gsub(",", "", ma_ck_data$`cao_nhat`))
  ma_ck_data$`thap_nhat` <- as.numeric(gsub(",", "", ma_ck_data$`thap_nhat`))
  
  # Tính trung bình các thuộc tính
  trung_binh <- colMeans(select(ma_ck_data, `khoi_luong`, `NN_mua`, `NN_ban`, `mo_cua`, `dong_cua`, `cao_nhat`, `thap_nhat`), na.rm = TRUE)
  
  return(trung_binh)
}



##tính trung vị 
tinh_trung_vi <- function(df, ma_ck) {
  # Lọc dữ liệu cho mã CK cụ thể
  ma_ck_data <- df %>% filter(`Ma_CK` == ma_ck)
  
  # Loại bỏ dấu phẩy và chuyển đổi các cột thành số
  ma_ck_data$`khoi_luong` <- as.numeric(gsub(",", "", ma_ck_data$`khoi_luong`))
  ma_ck_data$`NN_mua` <- as.numeric(gsub(",", "", ma_ck_data$`NN_mua`))
  ma_ck_data$`NN_ban` <- as.numeric(gsub(",", "", ma_ck_data$`NN_ban`))
  ma_ck_data$`cao_nhat` <- as.numeric(gsub(",", "", ma_ck_data$`cao_nhat`))
  ma_ck_data$`thap_nhat` <- as.numeric(gsub(",", "", ma_ck_data$`thap_nhat`))
  ma_ck_data$`mo_cua` <- as.numeric(gsub(",", "", ma_ck_data$`mo_cua`))
  ma_ck_data$`dong_cua` <- as.numeric(gsub(",", "", ma_ck_data$`dong_cua`))
  
  # Tính trung vị các thuộc tính
  trung_vi <- sapply(select(ma_ck_data, `khoi_luong`, `NN_mua`, `NN_ban`, `cao_nhat`, `thap_nhat`,`mo_cua`,`dong_cua`), quantile, prob = 0.5, na.rm = TRUE)
  
  return(trung_vi)
}


##tính độ lệch chuẩn
tinh_do_lech_chuan <- function(df, att) {
  # Loại bỏ dấu phẩy từ giá trị của cột "khoi_luong"
  if (att == "khoi_luong") {
    df[[att]] <- as.numeric(gsub(",", "", df[[att]]))
  }
  # Tính độ lệch chuẩn
  do_lech_chuan <- aggregate(df[[att]], by = list(df$`Ma_CK`), FUN = sd, na.rm = TRUE)
  colnames(do_lech_chuan) <- c("Ma_CK", paste(att))
  return(do_lech_chuan)
}

# Tạo DataFrame mới chứa độ lệch chuẩn của các thuộc tính cho từng mã CK
tinh_do_lech_chuan_atts <- function(df, atts) {
  do_lech_chuan_df <- data.frame("Ma_CK" = character(), stringsAsFactors = FALSE)
  for (att in atts) {
    do_lech_chuan_df <- merge(do_lech_chuan_df, tinh_do_lech_chuan(df, att), by = "Ma_CK", all = TRUE)
  }
  return(do_lech_chuan_df)
}

# Gọi hàm tính độ lệch chuẩn cho các thuộc tính
atts <- c("khoi_luong", "NN_mua", "NN_ban", "cao_nhat", "thap_nhat","mo_cua","dong_cua")
do_lech_chuan_df <- tinh_do_lech_chuan_atts(df_tong, atts)

##tính biên độ ( NN_mua - NN_ban, cao_nhat - thap_nhat)
tinh_bien_do <- function(df, att1, att2) {
  # Loại bỏ dấu phẩy từ giá trị 
  if (att1 %in% c("NN_mua", "NN_ban") && att2 %in% c("NN_mua", "NN_ban")) {
    df[[att1]] <- gsub(",", "", df[[att1]])
    df[[att2]] <- gsub(",", "", df[[att2]])
  }
  
  # Chuyển đổi sang số
  df[[att1]] <- as.numeric(df[[att1]])
  df[[att2]] <- as.numeric(df[[att2]])
  
  # Tính biên độ
  bien_do <- aggregate((df[[att1]] - df[[att2]]), by = list(df$`Ma_CK`), FUN = sd, na.rm = TRUE)
  colnames(bien_do) <- c("Ma_CK", paste( att1, "--", att2))
  return(bien_do)
}
# Tính toán biên độ cho các cặp thuộc tính
cac_cap_thuoc_tinh <- list(c("NN_mua", "NN_ban"), c("cao_nhat", "thap_nhat"))
bien_do_df <- data.frame("Ma_CK" = character(), stringsAsFactors = FALSE)
for (cap_thuoc_tinh in cac_cap_thuoc_tinh) {
  bien_do_df <- merge(bien_do_df, tinh_bien_do(df_tong, cap_thuoc_tinh[[1]], cap_thuoc_tinh[[2]]), by = "Ma_CK", all = TRUE)
}




# Hiển thị kết quả
tb_vcb <- tinh_trung_binh(df_tong, "vcb")
tb_vcb
tv_vcb <- tinh_trung_vi(df_tong,"vcb")
tv_vcb
print(bien_do_df)
print(do_lech_chuan_df)


# Hàm vẽ biểu đồ đường bằng ggplot2
ve_bieu_do_duong_ggplot <- function(df, ma_ck, bien) {
  # Lọc dữ liệu cho mã CK và biến cụ thể
  ma_ck_data <- df %>% filter(`Ma_CK` == ma_ck)
  
  # Chuyển cột Ngay thành dạng ngày tháng
  ma_ck_data$Ngay <- as.Date(ma_ck_data$Ngay, format = "%d/%m/%Y")
  
  # Biến đổi dấu phẩy và chuyển biến thành dạng số
  ma_ck_data[[bien]] <- as.numeric(gsub(",", "", ma_ck_data[[bien]]))
  
  # Tạo biểu đồ đường bằng ggplot2
  p <- ggplot(data = ma_ck_data, aes(x = Ngay, y = !!sym(bien))) +
    geom_line(color = "blue") +
    labs(title = paste("Biểu đồ đường biểu hiện sự phân phối cho mã CK'", ma_ck, "', Biến:", bien),
         x = "Ngày", y = bien)
  
  return(p)
}

#ham ve bd nen 
ve_bieu_do_nen_ggplot <- function(df, ma_ck) {
  # Lọc dữ liệu cho mã cổ phiếu cụ thể
  df_sub <- df[df$Ma_CK == ma_ck, ]
  
  # Tạo biểu đồ nến sử dụng ggplot2
  ggplot(data = df_sub, aes(x = Ngay, y = mo_cua, group = Ngay)) +
    geom_segment(aes(xend = Ngay, yend = dong_cua),
                 color = ifelse(df_sub$mo_cua > df_sub$dong_cua, "green", "red")) +
    geom_errorbar(aes(ymin = thap_nhat, ymax = cao_nhat),
                  width = 0.2, color = "black") +
    labs(x = "Ngày", y = "Giá", title = paste("Biểu đồ nến cho", ma_ck)) +
    theme_minimal()
}


# Hàm vẽ biểu đồ cột bằng ggplot2
ve_bieu_do_cot_ggplot <- function(df) {
  # Tạo biểu đồ cột bằng ggplot2
  p <- ggplot(data = df, aes(x = `Ma_CK`, y = `khoi_luong`, fill = `Ma_CK`)) +
    geom_bar(stat = "identity") +
    labs(title = "Phân phối khối lượng giao dịch trong 6 ngân hàng",
         x = "Mã cổ phiếu",
         y = "Khối lượng giao dịch") +
    theme_minimal()
  
  return(p)
}
# Hàm vẽ biểu đồ kết hợp cho SMA và biểu đồ nến của mã CK
ve_bieu_do_ke_hop_ggplot <- function(df, ma_ck, window1, window2) {
  # Lọc dữ liệu cho mã CK cụ thể
  ma_ck_data <- df %>% filter(Ma_CK == ma_ck) 
  
  # Tính đường SMA
  ma_ck_data$SMA1 <- SMA(ma_ck_data$dong_cua, n = window1)
  ma_ck_data$SMA2 <- SMA(ma_ck_data$dong_cua, n = window2)
  
  # Chuyển cột Ngay thành dạng ngày tháng
  ma_ck_data$Ngay <- as.Date(ma_ck_data$Ngay, format = "%d/%m/%Y")
  
  # Biến đổi dấu phẩy và chuyển các cột thành dạng số cho biểu đồ nến
  ma_ck_data$mo_cua <- as.numeric(gsub(",", "", ma_ck_data$mo_cua))
  ma_ck_data$cao_nhat <- as.numeric(gsub(",", "", ma_ck_data$cao_nhat))
  ma_ck_data$thap_nhat <- as.numeric(gsub(",", "", ma_ck_data$thap_nhat))
  ma_ck_data$dong_cua <- as.numeric(gsub(",", "", ma_ck_data$dong_cua))
  
  # Tạo biểu đồ kết hợp
  p <- ggplot(data = ma_ck_data, aes(x = Ngay)) +
    geom_segment(aes(y = mo_cua, yend = dong_cua,
                     color = ifelse(mo_cua > dong_cua, "green", "red"))) +
    geom_errorbar(aes(ymin = thap_nhat, ymax = cao_nhat),
                  width = 0.2, color = "black") +
    geom_line(aes(y = SMA1), color = "blue") +
    geom_line(aes(y = SMA2), color = "orange") +
    labs(title = paste("Biểu đồ kết hợp SMA và biểu đồ nến cho mã CK ", ma_ck),
         x = "Ngày", y = "Giá") +
    theme_minimal()
  
  return(p)
}

# Hàm vẽ biểu đồ histogram và đường MACD sử dụng ggplot2
ve_histogram_ggplot <- function(macd_df) {
  # Tạo biểu đồ histogram và đường MACD
  bieudo <- ggplot(macd_df, aes(x = Ngay)) +
    geom_bar(aes(y = abs(Histogram), fill = factor(Histogram > 0)), stat = "identity") +
    geom_line(aes(y = MACD_Line), color = "blue") +
    geom_line(aes(y = Signal_Line), color = "red") +
    labs(title = "Biểu đồ Histogram và Đường MACD",
         x = "Ngày", y = "Giá trị") +
    theme_minimal()
  
  return(bieudo)
}
#### vi du 1 vai ngan hang
#bd duong
bieu_do_duong_vpb <- ve_bieu_do_duong_ggplot(df_tong, "vpb", "khoi_luong")
bieu_do_duong_vpb
#bd nen
nen_vcb<-ve_bieu_do_nen_ggplot(df_tong,'vcb')
nen_vcb
#bd cot
cot_vcb<-ve_bieu_do_cot_ggplot(df_tong)
cot_vcb
#bd ket hop
bieu_do_ke_hop_vpb <- ve_bieu_do_ke_hop_ggplot(df_tong, "vpb", window1 = 10, window2 = 20)
bieu_do_ke_hop_vpb
#bd his
ve_histogram_ggplot(macd_vpb)

# Hàm vẽ biểu đồ MACD bằng ggplot2
ve_bieu_do_macd <- function(df, ma_ck) {
  # Lọc dữ liệu cho mã CK cụ thể
  ma_ck_data <- df %>% filter(Ma_CK == ma_ck)
  
  # Chuyển cột Ngay thành dạng ngày tháng
  ma_ck_data$Ngay <- as.Date(ma_ck_data$Ngay, format = "%d/%m/%Y")
  
  # Tạo biểu đồ MACD bằng ggplot2
  p <- ggplot(data = ma_ck_data, aes(x = Ngay)) +
    geom_line(aes(y = MACD_Line, color = "MACD Line")) +
    geom_line(aes(y = Signal_Line, color = "Signal Line")) +
    labs(title = paste("Biểu đồ MACD cho mã CK", ma_ck),
         x = "Ngày", y = "MACD",
         color = "Chỉ số") +
    theme_minimal()
  
  return(p)
}

# Vẽ biểu đồ MACD cho các ngân hàng
p_vcb_macd <- ve_bieu_do_macd(df_tong, "vcb")
p_cts_macd <- ve_bieu_do_macd(df_tong, "cts")
p_tcb_macd <- ve_bieu_do_macd(df_tong, "tcb")
p_msb_macd <- ve_bieu_do_macd(df_tong, "msb")
p_vpb_macd <- ve_bieu_do_macd(df_tong, "vpb")
p_bid_macd <- ve_bieu_do_macd(df_tong, "bid")

# Hiển thị các biểu đồ
gridExtra::grid.arrange(p_vcb_macd, p_cts_macd, p_tcb_macd, p_msb_macd, p_vpb_macd, p_bid_macd, ncol = 2)

