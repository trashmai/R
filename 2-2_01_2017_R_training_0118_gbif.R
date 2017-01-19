library(httr)
library(data.table)

#### 使用 NomenMatch 找出 GBIF Taxon Key ####
# 載入感興趣的物種學名們 (本 session 以鳥為例)
# 選擇工作目錄，載入檔案
# setwd(choose.dir())
names = fread("./data/aves_names.csv", sep = '\t', header = T)
# 或
#names = fread(choose.files(), sep = '\t', header = T)

# 以 pipe (|) 串接學名
names_string = paste(names$name, collapse = '|')
# 瞇一下看看
names_string

# 設定查詢 (POST) 參數
# names: 以 pipe (|) 串接的學名
# against: 與哪一個資料庫進行比對
# best: 是否只回傳最佳結果
# format: 回傳格式
post_data = list(
  names=names_string,
  against='gbif_backbone_txn',
  best='yes',
  format='json'
)

# NomenMatch API 服務位置
# 備用位置 query_url = "http://twebi.net/queryNames/api.php"
query_url = "http://match.taibif.tw/api.php"

# 使用 httr 套件的 POST function 執行 http POST method
res.content = content(POST(url=query_url, body=post_data, encode='form'))
# 分解動作
#res = POST(url=query_url, body=post_data, encode='form')
#res.content = content(POST(url=query_url, body=post_data, encode='form'))

# 看一下回傳結構
str(res.content$results[[1]])

# 提取 NomenMatch 的比對結果，包含比對分數、使用者輸入的學名、比對到的學名，與學名 id
matched.list = lapply(res.content$results, function (r) {
  # 為何資料是這樣拿? 請參考回傳結構 str(res.content$results[[1]])
  score = r[[1]]$score
  gbTxnKey = r[[1]]$best$gbif_backbone_txn
  gbTxnName = r[[1]]$matched_clean
  inputName = r[[1]]$name_cleaned
  if (!is.null(score) & !is.null(inputName) & !is.null(gbTxnName) & !is.null(gbTxnKey)) {
    data.table(score, inputName, gbTxnName, gbTxnKey)
  }
})
matched.dt = Reduce(rbind, matched.list)

# 看看是否有問題，data.table 才能這樣寫
matched.dt[score < 1]
# data.frame 請這樣寫
matched.dt[score < 1, ]

# 取出GBIF Taxon Key
# 有時會有多個亞種比到同一個種階層
# 這時便需要移除重複的 taxon key
id_list = unique(matched.dt$gbTxnKey)


#### 從 GBIF 下載資料 ####

# 設定從GBIF下載哪些資料欄位
fields = c('key','name', 'decimalLongitude', 'decimalLatitude', 'year')

# 載入 rgbif package
library(rgbif)
# 載入事先寫好，下載GBIF資料用的函式(function)，使用到 occ_search 函式 與 for 迴圈
# file.edit("function_2017_R_training_getGBIFData.R")
source("./include/function_2017_R_training_getGBIFData.R", encoding = 'UTF-8')

# 下載資料並存檔
#d = getGBIFData(id_list[1:5], fields, complete_only=T)
#d = as.data.table(d)
#write.table(d, file = './data/gbif_taiwan_aves.csv', fileEncoding = 'UTF-8', sep='\t', quote = F, row.names = F)
# 或
# 開啟歷史資料為data.table
d = fread('./data/gbif_taiwan_aves.csv', header = T, encoding = 'UTF-8', sep='\t')

# 看看資料摘要
summary(d)
# 經度 mininum 竟然是 23.5 很可疑? 調出經度最小的幾筆資料看看
d[order(decimalLongitude)][1:10,]
# 應該是資料有問題，剔除經度小於等於 100 的資料
d = d[decimalLongitude > 100]
# save(d, file='./cache/d.RData')

# 畫個地圖看看
gbifmap(d, region = 'Taiwan')

# 眼花去，R 抱怨調色盤只支援9種顏色 (rgbif 實作的問題)，我們就先來挑個 9 種物種

# 複習一下 data.table 的語法
# dt[符合篩選條件的row, 篩選或產生columns, 額外的參數如使用 by 分群]
# 操作結果一樣是 data.table ， 因此可接續使用中括號組 [] 做進一步操作
# 寫出來基本結構就會長得像 dt[][][][]...
# 例如用data.table的語法
# 依照物種計算出現紀錄筆數並排序
d.sorted = d[, .(sp_sum=length(decimalLongitude)), by='name'][order(sp_sum, decreasing = T)]
# 或是如果擔心資料裡面有 NA，可利用 R 會試圖轉換資料型態的特性用這招
#d.sorted = d[, .(sp_sum=sum(!is.na(decimalLongitude))), by='name'][order(sp_sum, decreasing = T)]

# 試試語法分解
# !is.na(d$decimalLongitude)
# sum(!is.na(d$decimalLongitude))
# d[, length(decimalLongitude)]
# d[, length(decimalLongitude), by='name']
# d[, .(sp_sum = length(decimalLongitude)), by='name']
# d[order(name, decreasing=T)]
# d[, .(sp_sum = length(decimalLongitude)), by='name'][order(sp_sum)]
# d[, .(sp_sum = length(decimalLongitude)), by='name'][order(sp_sum, decreasing = T)]

# 看看資料筆數前9名的物種
d.sorted[1:9]

#### 透過 key 合併兩個資料表單 (稱為 JOIN ) ####
# 把資料筆數前9名的物種挑選出來，並用它們的座標點位繪圖
# 語法程序
# setkey(dt1, keyvar_in_dt1)
# setkey(dt2, keyvar_in_dt2)
# dt1[dt2] 把dt1的所有資料接到dt2
# dt2[dt1] 反之
# 類似 excel 的 vlookup
# 參考資料 https://rstudio-pubs-static.s3.amazonaws.com/52230_5ae0d25125b544caab32f75f0360e775.html
# 以name為key，找出前9名的物種的分布座標
setkey(d, name)
setkey(d.sorted, name)
#
d.top9 = d[d.sorted[1:9]]
head(d.top9)

# 再畫看看
gbifmap(d.top9, region = 'Taiwan')

# 篩選特定物種
# e.g. 找學名符合特定pattern的資料
# grep 室內文比對功能，回傳比對到的資料的 index，例如
# Passer 開頭
d[grep('^Passer', name)]
# indicus 結尾
d[grep('indicus$', name)]
# 可拆解為
# matched_idx = grep(' indicus', d$name)
# d[matched_idx]
 
gbifmap(d[grep('indicus$', name)], region = 'Taiwan')


#### 用 ggmap 畫比較炫的地圖 ####
# 結合OpenStreetMap (最近改了認證方式暫時不能用) 或 Google Map 畫物種分布圖
# 參考資料: http://stackoverflow.com/questions/32206623/what-does-level-mean-in-ggplotstat-density2d

#install.packages('ggmap')
library(ggmap)

# 先畫個底圖看看
map = get_map(location = "Taiwan", color = 'bw', zoom = 7)
mapPoints = ggmap(map, extent = 'normal')
mapPoints

# 疊個密度圖跟點位
# 用法基本上跟 ggplot 一毛一樣
mapPoints +
  # 我是密度圖
  stat_density2d(data=d,aes(y=decimalLatitude, x=decimalLongitude, fill= ..level..), alpha =0.5, geom="polygon") +
  # 我是點位
  geom_point(data=d, aes(y=decimalLatitude, x=decimalLongitude), color='red', alpha = .2, pch='.')
  
# 如果顯示錯誤畫不出來, 請照以下流程處理移除 ggplot2，重開R，重新安裝開發版的 ggplot2
# Reference: http://stackoverflow.com/questions/40642850/ggmap-error-geomrasterann-was-built-with-an-incompatible-version-of-ggproto/40651687#40651687
#install.packages('devtools')
#library(devtools)
#remove.packages('ggplot2')
# *** restart R session before reinstall ggplot2 ***
#devtools::install_github('hadley/ggplot2')