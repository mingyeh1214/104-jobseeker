library(tidyverse)
library(RSelenium)
library(seleniumPipes)
library(rvest)
library(stringr)
library(openxlsx)
library(magick)

remDr <<- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome"
)

remDr$open()

main_url <- "https://www.edtung.com"
landing_url <- paste0(main_url, "/QuestionIndex.aspx") #題庫區

remDr$navigate(landing_url)

subject_radio_list <- list(
  math ="ctl00_ContentPlaceHolder1_UC_SearchOpions_Questions_Collapsible1_UC_SearchOptions_Questions1_rblSubject_1"
)

degree_radio_list <- list(
seven_up = "ctl00_ContentPlaceHolder1_UC_SearchOpions_Questions_Collapsible1_UC_SearchOptions_Questions1_rblSearchMapping_1"
,seven_down = "ctl00_ContentPlaceHolder1_UC_SearchOpions_Questions_Collapsible1_UC_SearchOptions_Questions1_rblSearchMapping_2"
,eight_up = "ctl00_ContentPlaceHolder1_UC_SearchOpions_Questions_Collapsible1_UC_SearchOptions_Questions1_rblSearchMapping_3"
,eight_down = "ctl00_ContentPlaceHolder1_UC_SearchOpions_Questions_Collapsible1_UC_SearchOptions_Questions1_rblSearchMapping_4"
,nine_up = "ctl00_ContentPlaceHolder1_UC_SearchOpions_Questions_Collapsible1_UC_SearchOptions_Questions1_rblSearchMapping_5"
,nine_down= "ctl00_ContentPlaceHolder1_UC_SearchOpions_Questions_Collapsible1_UC_SearchOptions_Questions1_rblSearchMapping_6"
)
search_btn = "ctl00_ContentPlaceHolder1_UC_SearchOpions_Questions_Collapsible1_UC_SearchOptions_Questions1_lbSearchQuestions"
next_btn ="ctl00_ContentPlaceHolder1_UC_QuestionList1_GridView1_ctl13_lbNextPage"

for(degree in names(degree_radio_list)[2:6]){
  
  remDr$navigate(landing_url)
  Sys.sleep(2)
  remDr$findElement(using = "id", value = subject_radio_list$math)$clickElement()
  Sys.sleep(1)
  remDr$findElement(using = "id", value = degree_radio_list[[degree]])$clickElement()
  Sys.sleep(1)
  remDr$findElement(using = "id", value = search_btn)$clickElement()
  Sys.sleep(1)
  

  page_info <- remDr$findElement(using = "xpath", value = '//*[@id="ctl00_ContentPlaceHolder1_UC_QuestionList1_GridView1"]/tbody/tr[12]/td/table/tbody/tr/td[1]')
  page_info <- page_info$getElementText()[[1]] %>%
    str_split(" ")

  total_page <- page_info[[1]][4] %>% as.integer()
  
  for(pageNum in 1:total_page){
    
 
  
  # 获取页面源代码
  page_source <- remDr$getPageSource()[[1]]
  
  # 使用rvest来解析页面源代码
  page <- read_html(page_source)
  
  # 提取所有的img标签
  img_tags <- html_nodes(page, "img")
  
  # 提取img标签的src属性
  img_urls <- html_attr(img_tags, "src")
  img_ids <- html_attr(img_tags, "id")
  img_ids <- img_ids[!is.na(img_ids)]
  # 过滤出以"/images/qimages/"开头的图片URL
  filtered_img_urls <- img_urls[str_detect(img_urls, "^/images/qimages/")]
  filtered_img_ids <- img_ids[str_detect(img_ids, "^ctl00_ContentPlaceHolder1_UC_QuestionList")]
  filtered_img_ids <-  data.frame(filtered_img_ids = filtered_img_ids)
  
  # 创建一个空的DataFrame来存储URL的各个部分
  url_df <- data.frame(matrix(nrow = length(filtered_img_urls), ncol = 9))
  colnames(url_df) <- c("img_url",  "folder1", "folder2", "folder3", "folder4", "folder5", "file6","file7",'file_name_full')
  
  # 将每个URL按照"/"切割，并保存到DataFrame中
  for (i in 1:length(filtered_img_urls)) {
    url_parts <- str_split(filtered_img_urls[i], "/")[[1]]
    url_df[i, ] <- c(filtered_img_urls[i]
                     , url_parts[length(url_parts) - 7]
                     , url_parts[length(url_parts) - 6]
                     , url_parts[length(url_parts) - 5]
                     , url_parts[length(url_parts) - 4]
                     , url_parts[length(url_parts) - 3]
                     , url_parts[length(url_parts) - 2]
                     , url_parts[length(url_parts) - 1]
                     , url_parts[length(url_parts)])
  }
  
  url_df <- url_df %>%
    mutate(url = paste0(main_url, img_url)) %>%
    mutate(file_name = paste0(sub('\\.jpg$', '', file_name_full))) %>%
    mutate(file_name_mod = paste0(file_name, '_', file7)) %>%
    mutate(file_path = paste0("math/",degree,"/", file_name_mod,".jpg")) %>%
    mutate(file_path_combine = paste0("math/",degree,"_combine/", file_name,".jpg")) %>%
    cbind(filtered_img_ids) %>%
    rowwise() %>%
    mutate(q_id = paste0(paste0(str_split(filtered_img_ids, "_")[[1]][1:6], collapse = "_"), "_L_RowNum")) %>%
    mutate(q_name = paste0(paste0(str_split(filtered_img_ids, "_")[[1]][1:6], collapse = "_"), "_L_QUnitName"))
  
  
  for(i in 1:dim(url_df)[1]){
    # 下载图片
    download.file(url = url_df$url[i], destfile = url_df$file_path[i], mode = "wb")
  }
  
  url_df <- url_df %>%
    mutate(width = image_info(image_read(file_path))$width) %>%
    mutate(height = image_info(image_read(file_path))$height)
  
  url_df_list <-url_df %>%
    split(.$file_name)
  
  
  for(image_num in 1:length(url_df_list)){
    imgs_url <- c(url_df_list[[image_num]]$file_path)
    combined_img <- image_montage(image_read(imgs_url
                                             , density = 600)
                                  , tile = '2x2',
                                  geometry = paste0('x', unique(url_df_list[[image_num]]$height),'+0+0'))
    
    q_id_value <- remDr$findElement(using = "id", value = unique(url_df_list[[image_num]]$q_id))$getElementText()[[1]]
    q_name_value <- remDr$findElement(using = "id", value = unique(url_df_list[[image_num]]$q_name))$getElementText()[[1]]
    q_name_value <- sub("：","_",q_name_value)
    url_df_list[[image_num]] <- url_df_list[[image_num]] %>%
      mutate(q_id_value = q_id_value) %>%
      mutate(q_name_value = q_name_value) %>%
      mutate(file_path_combine_mod = paste0("math/",degree,"_combine/",q_id_value,"_",q_name_value ,"_",file_name,".jpg"))
    
    
    image_write(combined_img, path = unique(url_df_list[[image_num]]$file_path_combine_mod), format = "jpg", density = 600)
  }
  
  
if(pageNum != total_page){
  
    remDr$findElement(using = "id", value = next_btn)$clickElement()
}
    Sys.sleep(2)
  }
  
  
  
  
}


