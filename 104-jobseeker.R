library(tidyverse)
library(RSelenium)
library(seleniumPipes)
library(rvest)
library(stringr)
library(openxlsx)

remDr <<- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome"
  )

remDr$open()

remDr$maxWindowSize()

# 工作性質
ro = c(
  0, # 全部
  1, # 全職
  2, # 兼職
  3  # 高階
  )[2]

# 搜尋關鍵字
keyword = "業務 AM"

# 只搜尋職務名稱
kwop = c(
  0, # 否 
  1  # 是
  )[2]

# 地區類別
area = c(
  "6001001000", # 台北市
  "6001002000", # 新北市
  "6001003000", # 宜蘭縣
  "6001004000", # 基隆市
  "6001005000", # 桃園市
  "6001006000", # 新竹縣市
  "6001007000", # 苗栗縣
  "6001008000", # 台中市
  "6001010000", # 彰化縣
  "6001011000", # 南投縣
  "6001012000", # 雲林縣
  "6001013000", # 嘉義縣市
  "6001014000", # 台南市
  "6001015000", # 南投縣
  "6001016000", # 高雄市
  "6001018000", # 屏東縣
  "6001019000", # 台東縣
  "6001020000", # 花蓮縣
  "6001021000", # 澎湖縣
  "6001022000", # 金門縣
  "6001023000"  # 連江縣
  )[1]

# 更新日期
isnew = c(
  0,  # 本日最新
  3,  # 三日內
  7,  # 一週內
  14, # 兩週內
  30  # 一個月內
  )[3]

mode = c(
  "s", # s為列表模式
  "l"  # l為清單模式
  )[2]

# 公司產業
indcat = c(
  "1001000000",   # 電子資訊／軟體／半導體相關業 
  "1001001000",  # 軟體及網路相關業
  "1001002000",  # 電信及通訊相關業
  "1001003000",  # 電腦及消費性電子製造業
  "1001004000",  # 光電及光學相關業
  "1001005000",  # 電子零組件相關業 
  "1001006000",  # 半導體業
  "1004000000",   # 金融投顧及保險業
  "1004001000",  # 金融機構及其相關業
  "1004002000",  # 投資理財相關業
  "1004003000",  # 保險業
  "1008003000"   # 顧問╱研發╱設計業
  )[4]

indcat_ <-
  case_when(
    indcat == "1001000000" ~ "電子資訊／軟體／半導體相關業",
    indcat == "1001001000" ~ "軟體及網路相關業",
    indcat == "1001002000" ~ "電信及通訊相關業",
    indcat == "1001003000" ~ "電腦及消費性電子製造業",
    indcat == "1001004000" ~ "光電及光學相關業",
    indcat == "1001005000" ~ "電子零組件相關業",
    indcat == "1001006000" ~ "半導體業",
    indcat == "1004000000" ~ "金融投顧及保險業",
    indcat == "1004001000" ~ "金融機構及其相關業",
    indcat == "1004002000" ~ "投資理財相關業",
    indcat == "1004003000" ~ "保險業",
    indcat == "1008003000" ~ "顧問╱研發╱設計業",
    TRUE ~ as.character(indcat)
  )

# 上班時段
s9 <- c(
  1, # 日班
  2, # 晚班
  4, # 大夜班
  8  # 假日班
  )[1]

# 周休二日
wktm <- c(
  0, # 否
  1  # 是
  )[1]

recommendJob <- c(0, 1)[1]

hotJob <- c(0, 1)[1]

langFlag <- 0

langStatus <- 0

if(length(area) != 1){
  area <- paste(area, collapse = "%2C")
}

my_params = paste0(
  "&ro=", ro,
  "&keyword=", keyword,
  "&area=", area,
  "&isnew=", isnew, 
  "&mode=", mode,
  "&indcat=", indcat,
  "&s9=", s9,
  "&kwop=", kwop,
  "&wktm=", wktm,
  "&langFlag=", langFlag,
  "&langStatus=", langStatus,
  "&recommendJob=", recommendJob,
  "&hotJob=", hotJob
  )

main_url <- "https://www.104.com.tw/jobs/search/?"

url <- paste0("https://www.104.com.tw/jobs/search/?", my_params)

remDr$navigate(url)

valid_pages <-
  remDr$findElement(using = "xpath", '//*[contains(concat( " ", @class, " " ), concat( " ", "gtm-paging-top", " " ))]')$getElementText()[[1]] %>%
  strsplit(' ') %>%
  lapply(as.integer) %>%
  vapply(max, integer(1L), na.rm = TRUE)

print(valid_pages)

get.job_links <- function(){
  page_source <-
    remDr$getPageSource()[[1]]
  
  job_links <-
    paste0("https://www.104.com.tw/job/", 
           str_extract_all(page_source,"(?<=www.104.com.tw/job/).+(?=jobsource=jolist)")[[1]],
           "jobsource=jolist_d_relevance"
    )
  return(job_links)

}

get.company_links <- function(){
  page_source <-
    remDr$getPageSource()[[1]]
  

  company_links <-
    paste0("https://www.104.com.tw/company/", 
           str_extract_all(page_source,"(?<=www.104.com.tw/company/).+(?=jobsource=jolist)")[[1]],
           "jobsource=jolist_d_relevance"
    )
  return(company_links)
}

main.crawler <- function(job_links, company_links){
  
  job_table <- data.frame(
    key = NA,
    link = NA,
    工作職稱 = NA,
    更新日期 = NA,
    公司名稱 = NA,
    產業類別 = NA,
    員工人數 = NA,
    上班地點 = NA,
    應徵人數 = NA,
    待遇內容 = NA,
    需求人數 = NA,
    工作經歷 = NA,
    學歷要求 = NA,
    工作內容 = NA,
    福利內容 = NA,
    其他條件 = NA
  )

  for(i in 1:length(job_links)){
    print(paste0(i, "/", length(job_links)))
    Sys.sleep(0.5)
    remDr$navigate(job_links[i])
    html <- remDr$getPageSource()[[1]] %>%
      read_html
    key <- str_match(job_links[i], "job/\\s*(.*?)\\s*\\?jobsource")[,2]
    link <- job_links[i]
    工作職稱 <-
      html %>% 
      html_nodes("h1.pr-6.text-break") %>% 
      html_attr("title")
    if(length(工作職稱) ==0){
      工作職稱 <- NA
    }
    更新日期 <-
      html %>% 
      html_nodes(".ml-3.t4.text-gray-darker") %>%
      html_nodes("span") %>%
      html_attr("title")
    if(length(更新日期) ==0){
      更新日期 <- NA
    }
    公司名稱 <-
      html %>% 
      html_nodes("div.mt-3") %>%
      html_nodes("a.btn-link.t3.mr-6") %>% 
      html_attr("title") %>%
      .[1]
    if(length(公司名稱) ==0){
      公司名稱 <- NA
    }
    應徵人數 <-
      html %>%
      html_nodes("a.font-weight-bold.d-inline-block.pl-2.align-middle") %>% 
      html_text()
    if(length(應徵人數) ==0){
      應徵人數 <- NA
    }
    工作內容 <-
      html %>%
      html_nodes("p.job-description__content") %>% 
      html_text()
    if(length(工作內容) ==0){
      工作內容 <- NA
    }
    待遇內容 <-
      html %>%
      html_nodes(".t3.mb-0.mr-2.text-primary.font-weight-bold.align-top.d-inline-block") %>% 
      html_text()
    if(length(待遇內容) ==0){
      待遇內容 <- NA
    }
    福利內容 <-
      html %>%
      html_nodes(".r3.mb-0.text-break") %>% 
      html_text()
    if(length(福利內容) ==0){
      福利內容 <- NA
    }
    上班地點 <-
      html %>%
      html_nodes(".job-address") %>% 
      html_text()
    if(length(上班地點) ==0){
      上班地點 <- NA
    }
    其他條件 <-
      html %>%
      html_nodes("p.m-0.r3.w-100") %>% 
      html_text()
    if(length(其他條件) ==0){
      其他條件 <- NA
    }
    jd <-html %>%
      html_nodes("div.col.p-0.list-row__data") %>%
      html_nodes("p.t3.mb-0") %>%
      html_text() %>%
      str_trim(side = c("both")) 
    jd <- jd[!jd %in% ""] # 去除空白項目
    jd <- jd[!jd %in% "（經常性薪資達 4 萬元或以上）"] # 去除當面議時多出來的項目
    jd <- jd[!grepl("遠端",jd)] # 去除遠端工作項目
    需求人數 <-
      jd %>%
      .[8]
    工作經歷 <-
      jd%>%
      .[9]
    學歷要求 <-
      jd %>%
      .[10]
    remDr$navigate(company_links[i])
    html <- remDr$getPageSource()[[1]] %>%
      read_html
    產業類別 <- html %>%
      html_nodes("p.t3.mb-0") %>%
      html_text() %>%
      .[1]
    員工人數 <- html %>%
      html_nodes("p.t3.mb-0") %>%
      html_text() %>%
      .[7]
    job_table <- job_table %>%
      bind_rows(
        data.frame(
          key = key,
          link = link,
          工作職稱 = 工作職稱,
          更新日期 = 更新日期,
          公司名稱 = 公司名稱,
          產業類別 = 產業類別,
          員工人數 = 員工人數,
          上班地點 = 上班地點,
          應徵人數 = 應徵人數,
          工作內容 = 工作內容,
          待遇內容 = 待遇內容,
          福利內容 = 福利內容,
          需求人數 = 需求人數,
          工作經歷 = 工作經歷,
          學歷要求 = 學歷要求,
          其他條件 = 其他條件
        )
      )
  }
  return(job_table)
}

get_job_links_list <- list()
get_company_links_list <- list()
div <- valid_pages %/% 14
mod <- valid_pages %% 14

if(div != 0){
  for(i in 1:div){
    print(paste0("第", i, "頁"))
    for(j in 1:14){
      webElem <- remDr$findElement("css", "body")
      webElem$sendKeysToElement(list(key = "end"))
      Sys.sleep(2)
    }
    get_job_links_list[[i]] <- get.job_links()
    get_company_links_list[[i]] <- get.company_links()
    remDr$refresh()
  }
  for(k in 1:mod){
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end"))
    Sys.sleep(2)
  }
  get_job_links_list[[i+1]] <- get.job_links()
  get_company_links_list[[i+1]] <- get.company_links()
}else{
  for(k in 1:14){
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end"))
    Sys.sleep(2)
  }
  get_job_links_list[[1]] <- get.job_links()
  get_company_links_list[[1]] <- get.company_links()
}

job_links <- do.call(rbind, map(get_job_links_list, data.frame))
company_links <- do.call(rbind, map(get_company_links_list, data.frame))

links <- data.frame(
  job_links,
  company_links
)
colnames(links) <- c("job_links", "company_links")
links <- unique(links[,1:2])

job_table <- main.crawler(links$job_links, links$company_links)
as.numeric(gsub(".*?([0-9]+).*", "\\1",  "月薪40,000元以上"))

job_table_ <- job_table %>%
  filter(!is.na(工作職稱)) %>%
  filter(工作職稱 != "") %>%
  mutate(更新日期 = gsub("更新", "", 更新日期)) %>%
  mutate(更新日期 = as.Date(更新日期)) %>%
  mutate(應徵人數 = gsub("人應徵", "", 應徵人數)) %>%
  mutate(待遇內容 = ifelse(待遇內容 == " 待遇面議 ", " 月薪40,000元以上", 待遇內容)) %>%
  mutate(需求人數 = gsub("人", "", 需求人數)) %>%
  mutate(學歷要求 = gsub("以上", "", 學歷要求)) %>%
  mutate(行政區域 = substr(上班地點, 1, 7)) %>%
  mutate_at(vars(工作職稱, 公司名稱, 產業類別, 員工人數, 應徵人數, 待遇內容), str_trim) %>%
  mutate(是否已投 = "否") %>%
  mutate(自動投遞 = "否") %>%
  mutate(員工人數 = ifelse(員工人數 == "暫不提供", NA, 員工人數)) %>% 
  mutate(員工人數 = gsub("人", "", 員工人數)) %>%
  rowwise() %>%
  mutate(check_月薪 = ifelse(isTRUE(grepl("月薪",待遇內容)), TRUE, FALSE)) %>%
  mutate(check_年薪 = ifelse(isTRUE(grepl("年薪",待遇內容)), TRUE, FALSE)) %>%
  mutate(最低待遇 = strsplit(待遇內容, split = "~")[[1]][1]) %>%
  mutate(最低待遇 = gsub(",", "", 最低待遇)) %>%
  mutate(最低待遇 = as.numeric(gsub(".*?([0-9]+).*", "\\1", 最低待遇))) %>%
  mutate(最高待遇 = strsplit(待遇內容, split = "~")[[1]][2]) %>%
  mutate(最高待遇 = gsub(",", "", 最高待遇)) %>%
  mutate(最高待遇 = as.numeric(gsub(".*?([0-9]+).*", "\\1", 最高待遇))) %>%
  ungroup() %>%
  mutate(check_薪資結構 = check_月薪 + check_年薪) %>%
  filter(check_薪資結構 != 0) %>%
  mutate(薪資結構 = substr(待遇內容, 1, 2)) %>%
  select(link, 
         是否已投, 
         自動投遞, 
         工作職稱, 
         公司名稱, 
         產業類別, 
         員工人數, 
         行政區域, 
         需求人數, 
         應徵人數, 
         薪資結構, 
         最低待遇, 
         最高待遇, 
         工作經歷, 
         學歷要求, 
         工作內容, 
         其他條件, 
         福利內容)

title <- paste0("104職缺_", keyword, "_", area, "_", isnew, "日內更新_", indcat_, "_", Sys.Date())
wb <- createWorkbook(title = title)
modifyBaseFont(wb, fontSize = 10, fontColour = "#FFFFFF", fontName = "微軟正黑體")
addWorksheet(wb,sheetName = keyword)
writeDataTable(wb,sheet = keyword,x = job_table_)
saveWorkbook(wb, paste0(title, ".xlsx"), overwrite = TRUE)



