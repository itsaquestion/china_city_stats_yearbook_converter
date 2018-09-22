library(rvest)
library(tidyverse)
library(XLConnect)
library(readxl)

lsr::rmAll(F)
source("R/convert_cnstats_df.R",encoding = "gbk")
source("R/unspan_header.R")

#data_dir = "./demo_data"
#full_path = file.path(data_dir,"error4.xls")

is_real_xls = function(x){
  tryCatch({
    readWorksheetFromFile(x,sheet = 1)
    TRUE},
    error=function(err) FALSE)
}

check_and_load = function(x){
  # 测试数据的真实格式
  # 原版文件是html伪装的xls，
  # 某些文件，被加入广告之后，是"真正的xls"文件，
  # 最后只读取具有城市数据的
  cat(paste(">> ",x,"\n"))
  full_path = file.path(data_dir,x)
  
  df = NULL
  if(is_real_xls(full_path)){
    df = convert_cnstats_by_xls(full_path)
  }else{
    df = convert_cnstats_by_html(full_path)
  }
  if("城市" %in% names(df)){
    return(df)
  }
  NA
 
}



check_redundant_cities = function(table_result){
  # merge
    length(unique(table_result$城市)) == nrow(table_result)
}
  
check_redundant_colnames = function(table_result){
  #  重复的列，merge会加上.x和.y的后缀，这里检查是否存在
  !any(names(table_result) %>% 
        sapply(function(x){
          grepl("\\.x",x)
        }))
}

filter_files_from_name = function(table_name){
  ii = sapply(xls_file_list,get_table_name_from_file_name) == table_name
  xls_file_list[ii]
}
  

get_table_name_from_file_name = function(file_name){
  #str_extract_all(file_name)
  number_str = tail(str_extract_all(file_name,"\\((.*?)\\)")[[1]],1)
  if(length(number_str)==0){return(file_name)}
  str_remove_all(file_name,number_str) %>%
    str_remove_all("\\(\\)\\.xls")
}


compact_table_of_redundant_cities = function(table_result){
  # 用于合并类似以下下格式：
  #
  # 广州, 123, NA
  # 广州, NA, 456
  #
  # 合并结果为：
  #
  # 广州, 123, 456
  #
  all_cities = unique(table_result$城市)
  
  result = map_dfr(all_cities,function(city){
    #print(city)
    df = filter(table_result,城市 == city)
    values = map_df(df,function(a_row){
      a_row = a_row[!is.na(a_row)]
      
      if(length(a_row)==0){return(NA_real_)}
      
      value = unique(a_row)
      if(length(value)>1){
        if(length(unique(value))==1){
            value = value[1]
        }else{
          stop("redundant number")
        }
      }
      value
    })
    values
  })
  
  result
}


write_csv_gbk = function(x,file_path){
  con <- file(file_path,encoding="gbk")
  write.csv(x,file=con,row.names = F)
}


join_by_city= function(x, y){
  suppressWarnings(
    {
      if(is.na(x)){return(y)}
      if(is.na(y)){return(x)}
    }
  )
  #x = as.data.frame(x)
  #y = as.data.frame(y)
  
  merge(x,y,all=T)
}

# =========================================================================

data_dir = "C:/Users/lee/Desktop/2015"

output_path = file.path(data_dir,"output")
if(!dir.exists(output_path)){dir.create(output_path)}

xls_file_list = list.files(data_dir,pattern="*\\.xls$") %>% unname()

table_name_list = sapply(xls_file_list,get_table_name_from_file_name) %>% 
  unique()

walk(table_name_list,function(table_name){
  
  print(table_name)
  table_sub_file_list = filter_files_from_name(table_name) %>%
    map(~check_and_load(.))
  
  if(all(is.na(table_sub_file_list))){return(NULL)}
  
  table_result = reduce(table_sub_file_list,join_by_city)
  head(table_result)
  
  if(!check_redundant_colnames(table_result)){
    stop(paste0("redundant_colnames:", table_name))
  }
  
  
  if(!check_redundant_cities(table_result)){
    #stop(paste0("redundant_cities:", table_name))
    table_result = compact_table_of_redundant_cities(table_result)
  }
  
  output_file_name = paste0(table_name,".csv",sep="")
  
  output_file_path = file.path(output_path,output_file_name)
  write_csv_gbk(table_result,output_file_path)
})

