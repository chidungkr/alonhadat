
library(tidyverse)
library(rvest)
library(stringr)
library(magrittr)

#=====================================================================
#  Phần 1: Cào dữ liệu và lấy dữ liệu địa lí cho các khối bất động sản
#======================================================================


# Hàm lấy thông tin về khối bất động sản kiểu 1: 

get_information_for_estates1 <- function(link) {
  m <- read_html(link)
  p_gia <- '//*[@id="left"]/div[1]/div[3]/span[1]/span[2]'
  
  gia <- html_nodes(m, xpath = p_gia) %>% html_text()
  
  p_dien_tich <- '//*[@id="left"]/div[1]/div[3]/span[2]/span[2]'
  dien_tich <- html_nodes(m, xpath = p_dien_tich) %>% html_text()
  
  p_dia_chi <- '//*[@id="left"]/div[1]/div[4]/span[2]'
  dia_chi <- html_nodes(m, xpath = p_dia_chi) %>% html_text()
  
  
  addi <- html_nodes(m, xpath = '//*[@id="left"]/div[1]/div[5]/div[2]/table') %>% 
    html_table(fill = TRUE) %>% 
    .[[1]]
  
  my_vec <- c(gia, dien_tich, dia_chi, addi$X2[-6], addi$X4[-6], addi$X6)
  my_vec <- my_vec %>% 
    matrix(ncol = 19, byrow = TRUE) %>% 
    as.data.frame() 
  
  names(my_vec) <- c("gia", "dien_tich", "dia_chi", 
                     "code", "loai_tin", "loai_bds", "ngang", "dai", 
                     "huong", "duong_rong", "phap_ly", "so_tang", "phong_ngu", 
                     "phong_an", "nha_bep", "san_thuong", "de_xe", "chinh_chu", "ten_du_an")
  
  return(my_vec)
  
}


# Hàm lấy các thông tin về khối bất sản kiểu 2: 

get_information_for_estates2 <- function(link) {
  
  link %>% 
    read_html() -> m
  
  m %>% 
    html_nodes('.ct_dt') %>% 
    html_text() -> dien_tich 
  
  m %>% 
    html_nodes('.ct_price') %>% 
    html_text() -> gia
  
  m %>% 
    html_nodes('.ct_dis') %>% 
    html_text() -> dia_chi
  
  m %>% 
    html_nodes('.ct_direct') %>% 
    html_text() -> huong
  
  my_df <- data.frame(dien_tich = dien_tich, 
                      gia = gia, 
                      dia_chi = dia_chi, 
                      huong = huong)
  return(my_df %>% mutate(link_source = link))
  
}

test_link <- "https://alonhadat.com.vn/nha-dat/can-ban/can-ho-chung-cu/1/ha-noi/trang--102.html#"

# Test thời gian để thấy rằng lấy dữ liệu là không quá mất thời gian: 

library(microbenchmark)

microbenchmark(
  get_information_for_estates2(test_link), 
  times = 10
)

# Lấy dữ liệu: 

all_df <- lapply(paste0(paste0("https://alonhadat.com.vn/nha-dat/can-ban/can-ho-chung-cu/1/ha-noi/trang--", 1:102), 
                        ".html#"), get_information_for_estates2)

all_df <- do.call("bind_rows", all_df)
all_df %<>% mutate_if(is.factor, as.character)

# Hàm lấy dữ liệu địa lí  khi biết trước địa chỉ: 

library(ggmap)


get_spatial_data <- function(x) {
  result <- geocode(x, output = "latlona", source = "google")
  return(result %>% mutate(origin_add = x))
}

# Lấy dữ liệu địa lí của các khối BĐS: 

u <- lapply(all_df$dia_chi, get_spatial_data)
u <- do.call("bind_rows", u)


# Hợp nhất dữ liệu: 

all_df %<>% mutate(lon = u$lon, lat = u$lat)

# Lưu lại dữ liệu: 
write.csv(all_df, "D:/bat_dong_san.csv", row.names = FALSE)


#=====================================
#  Phần 2: Xử lí dữ liệu và báo cáo
#=====================================

# Đọc dữ liệu (nếu đã tắt máy tính): 

all_df <- read.csv("D:/bat_dong_san.csv") %>% 
  mutate_if(is.factor, as.character)


# Viết hàm xử lí thông tin về diện tích: 


get_area <- function(x) {
  x %>% 
    str_remove_all("m.*$") -> m
  
  m %>% 
    str_split(":") %>% 
    unlist() %>% matrix(ncol = 2, byrow = TRUE) %>% 
    as.data.frame() %>% 
    pull(V2) %>% 
    as.character() %>% 
    as.numeric() %>% 
    return()
    
}

# Test Hàm: 
all_df$dien_tich[1:7] %>% get_area()


# Hàm xử lí thông tin về giá: 

get_price <- function(x) {
  dt <- all_df$dien_tich %>% get_area()
  
  xuat_hien_m2 <- str_detect(x, "/")
  xuat_hien_trieu <- str_detect(x, "tr") & !str_detect(x, "/") & !str_detect(x, "t<")
  xuat_hien_ty <- str_detect(x, "t<")
  
  x1 <- x
  x2 <- x
  x3 <- x
  
  # Xử lí 1: 
  x1[!xuat_hien_m2] <- 0 
  x2[!xuat_hien_trieu] <- 0
  x3[!xuat_hien_ty] <- 0
  
  x1 %>% 
    str_sub(1, 10) %>%
    str_replace_all("[^0-9]", "") -> x1
  
  x1[str_count(x1) == 2] <- paste0(x1[str_count(x1) == 2], "0")
  x1 <- as.numeric(x1) / 10
  
  x2 %>% 
    str_sub(1, 12) %>%
    str_replace_all("[^0-9]", "") %>% 
    as.numeric() / dt -> x2
  
  
  x3 <- x3 %>% 
    str_sub(1, 12) %>%
    str_replace_all("[^0-9]", "")
  
  x3[str_count(x3) == 1] <- paste0(x3[str_count(x3) == 1], "000")
  x3[str_count(x3) == 2] <- paste0(x3[str_count(x3) == 2], "00")
  x3[str_count(x3) == 3] <- paste0(x3[str_count(x3) == 3], "0")
  
  x3 <- x3 %>% as.numeric() / dt
  return(round(x1 + x2 + x3, 2))
  
}


# Test hàm + kiểm tra: 

get_price(all_df$gia) %>% head(10)
all_df$dien_tich %>% head(10)
all_df$gia %>% head(10)

# Hàm xử lí thông tin về quận: 

get_district <- function(x) {
  ELSE <- TRUE
  case_when(str_detect(x, "Thanh Xuân") ~ "Thanh Xuân", 
            str_detect(x, "Nam") & str_detect(x, "Liêm") ~ "Nam Từ Liêm", 
            !str_detect(x, "Nam") & str_detect(x, "Liêm") ~ "Bắc Từ Liêm", 
            str_detect(x, "Hà Ðông") ~ "Hà Đông", 
            str_detect(x, "Thanh Trì") ~ "Thanh Trì", 
            str_detect(x, "Hoàng Mai") ~ "Hoàng Mai", 
            str_detect(x, "Long Biên") ~ "Long Biên", 
            str_detect(x, "Hai Bà Trung") ~ "Hai Bà Trưng", 
            str_detect(x, "Ba Ðình") ~ "Ba Đình", 
            str_detect(x, "ng Ða") ~ "Đống Đa", 
            str_detect(x, "Ba Ðình") ~ "Ba Đình", 
            str_detect(x, ">u Gi<") ~ "Cầu Giấy", 
            str_detect(x, "n Tây") ~ "Hồ Tây", 
            ELSE ~ "Khu Vực Khác")
}

# Sử dụng các hàm ở trên: 

all_df_pre <- all_df %>% 
  mutate(price = get_price(gia), 
         area = get_area(dien_tich), 
         district = get_district(dia_chi))

all_df_pre %<>% 
  filter(area > 35 & area < 150) %>% 
  filter(price > 5 & price < 50)

theme_set(theme_minimal())

all_df_pre %>% 
  group_by(district) %>% 
  count() %>% 
  ggplot(aes(reorder(district, n), n)) + 
  geom_col(fill = "#104E8B") + 
  coord_flip() + 
  geom_text(aes(label = n), hjust = 1.2, color = "white", size = 5) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.y = element_text(face = "bold", size = 10, color = "#104E8B"), 
        plot.subtitle = element_text(size = 12), 
        plot.title = element_text(size = 20)) + 
  labs(x = NULL, y = NULL, 
       title = "Number of Apartments for Sale by District", 
       caption = "https://alonhadat.com.vn")


# VỊ trí các dự án bất động sản của các căn hộ được rao bán:  

map <- get_map(location = "Hanoi", zoom = 12)
ggmap(map) + 
  geom_point(data = all_df_pre, 
             aes(x = lon, y = lat), color = "red", size = 3, alpha = 0.4) + 
  theme(axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white")) + 
  labs(title = "Location of Apartments for Sales", 
       subtitle = "Data Source: https://alonhadat.com.vn")



# ggmap(map) + 
#   geom_point(data = all_df_pre, 
#              aes(x = lon, y = lat, color = district), alpha = 0.4, size = 4) + 
#   theme(axis.title.x = element_text(colour = "white"),
#         axis.title.y = element_text(colour = "white"),
#         axis.text.x = element_text(colour = "white"),
#         axis.text.y = element_text(colour = "white")) + 
#   labs(title = "Location of Apartments for Sales", 
#        subtitle = "Data Source: https://alonhadat.com.vn")


all_df_pre %>% 
  group_by(district) %>% 
  summarise_each(funs(mean, median, min, max, n()), price) %>% 
  ungroup() %>% 
  arrange(-price_mean) %>% 
  mutate_if(is.numeric, function(x) {round(x, 1)}) -> price_dis

knitr::kable(price_dis, col.names = c("District", "Mean", "Median", "Min", "Max", "N"))


price_dis %>% 
  ggplot(aes(reorder(district, price_mean), price_mean)) + 
  geom_col(fill = "#104E8B") + 
  coord_flip() + 
  geom_text(aes(label = price_mean), hjust = 1.2, color = "white", size = 5) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.y = element_text(face = "bold", size = 10, color = "#104E8B"), 
        plot.subtitle = element_text(size = 12), 
        plot.title = element_text(size = 20)) + 
  labs(x = NULL, y = NULL, 
       title = "Mean Price per Square Meter of the Apartment by District", 
       caption = "https://alonhadat.com.vn")

small_df <- all_df_pre %>% 
  select(price, area, district) %>% 
  mutate(total_price = price*area)



small_df %>% 
  group_by(district) %>% 
  summarise_each(funs(mean, median, min, max, n()), total_price) %>% 
  ungroup() %>% 
  arrange(-total_price_mean) %>% 
  mutate_if(is.numeric, function(x) {round(x, 0)}) -> df1
  
knitr::kable(df1, col.names = c("District", "Mean", "Median", "Min", "Max", "N"))


df1 %>% 
  ggplot(aes(reorder(district, total_price_mean), total_price_mean)) + 
  geom_col(fill = "#104E8B") + 
  coord_flip() + 
  geom_text(aes(label = total_price_mean), hjust = 1.2, color = "white", size = 5) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.y = element_text(face = "bold", size = 10, color = "#104E8B"), 
        plot.subtitle = element_text(size = 12), 
        plot.title = element_text(size = 20)) + 
  labs(x = NULL, y = NULL, 
       title = "Mean Price of the Apartment by District", 
       caption = "https://alonhadat.com.vn")



small_df %>% 
  group_by(district) %>% 
  summarise_each(funs(mean, median, min, max, n()), area) %>% 
  ungroup() %>% 
  arrange(-area_mean) %>% 
  mutate_if(is.numeric, function(x) {round(x, 0)}) %>% 
  ggplot(aes(reorder(district, area_mean), area_mean)) + 
  geom_col(fill = "#104E8B") + 
  coord_flip() + 
  geom_text(aes(label = area_mean), hjust = 1.2, color = "white", size = 5) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.y = element_text(face = "bold", size = 10, color = "#104E8B"), 
        plot.subtitle = element_text(size = 12), 
        plot.title = element_text(size = 20)) + 
  labs(x = NULL, y = NULL, 
       title = "The Average Area of the Apartment by District", 
       subtitle = "Unit used: Square Meters (m2)", 
       caption = "https://alonhadat.com.vn")
  
