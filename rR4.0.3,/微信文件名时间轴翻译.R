# Tue Aug 30 23:28:55 2022 edit
# 字符编码：UTF-8
# R 版本：R 4.2.1 x64 for window 11
# cgh163email@163.com
# 个人笔记不负责任，拎了个梨????
#.rs.restartR()
#
require(showtext)
library(fs)

require(stringr)
require(lubridate)
require(tidyr)
rm(list = ls());gc()

showtext_auto()


system("tree") #通过系统命令查看目录结构


#处理时间 Tue Aug 30 23:33:03 2022 ------------------------------

# 当前目录文件列表
f.ls <- dir_ls(glob = '*.jpg$')
f.ls |> head()

tmp <-
f.ls |>
  str_extract_all('[0-9]') |>
  lapply(data.frame) #|>
  # do.call(rbind)
tmp <-   do.call(rbind,lapply(f.ls |> str_extract_all('[0-9]'), str_c))
# tmp2 <-
tmp |>
   as.data.frame() |>
  unite('new',sep = '') |>
  head() #|>

  op <- options(digits.secs = 4)  #设置精确度，最后记得调回默认
  z <- 1614431534883
  # avoid rounding down: milliseconds are not exactly representable
  as.POSIXct((z+0.1)/1000, origin = "1970-01-01")
# Wed Aug 31 00:39:16 2022 ---

  tmp |>
    as.data.frame() |>
    unite('new',sep = '') |>
    head() |>
    apply(2,as.numeric) |>
apply(1,( function(k) sum(k,.1)/1000 ) ) |>
    as.POSIXct( origin = "1970-01-01",tz = 'GMT') |>
    as.character.Date() |>
str_replace_all(':','_')

options(op) #  调回默认精确度
# Wed Aug 31 01:59:31 2022 ---
op <- options(digits.secs = 4)  #设置精确度，最后记得调回默认

stime2mytime <- function(text_time) {
  text_time |>
    str_sub(text_time |> str_length()- 16,
            text_time |> str_length()-4) |>  #时间轴
    as.data.frame() |>
    unite('new',sep = '') |>
    # head() |>
    apply(2,as.numeric) |>
    apply(1,( function(k) sum(k,.1)/1000 ) ) |>
    as.POSIXct( origin = "1970-01-01",tz = 'GMT') |>
    as.character.Date() |>
    str_replace_all(':','_')
}

stime2mytime(f.ls[1:50])


options(op) #  调回默认精确度

#重命名草稿 Wed Aug 31 01:10:26 2022 ------------------------------
fname2timef <- function(texts,type) {
# texts <- as.data.frame(texts)
  str_c(
  texts |> as.character() |>
  # str_length()
  str_sub(1,texts |> str_length()- 17)
,stime2mytime(texts)
,paste0('.',type))
}
fname2timef(f.ls,'jpg')


#正式版 Wed Aug 31 01:09:46 2022 ------------------------------
library(fs)
require(stringr)
require(lubridate)
require(tidyr)

ren_run <- function(type) {
f.ls <- dir_ls(glob = paste0('*.',type,'$'));f.ls |> head()

ok.dt <-
  cbind(f.ls,fname2timef(texts = f.ls,type = type))

  ok.dt <-
    ok.dt[!is.na(ok.dt[,2]), ]
  rm(f.ls)
  write.csv(ok.dt,file = 'renamedata.csv')
file.rename(ok.dt[,1],ok.dt[,2])

}
#
# file.choose(new = FALSE) |> setwd()
ren_run(type = 'jpg');  kittyR::meowR(sound = 4)


