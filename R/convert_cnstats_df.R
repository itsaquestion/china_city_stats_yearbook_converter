is_last_row_all_na = function(df){
  all(is.na(tail(df[,-1],1)))
}

remove_empty_last_row = function(df){
  if(is_last_row_all_na(df)){
    df = df[-nrow(df),]
  }
  df
}


is_a_data_row = function(a_row,percent = 0.5){
  # 如果能转化为数字的列，比例超过percetn，那么就是一个数据列（而不是标题等）
  suppressWarnings(
    mean(!is.na(as.numeric(a_row))) >= percent
  )
}

find_first_data_row = function(df){
  for(i in 1:nrow(df)){
    if(is_a_data_row(df[i,-1])){
      break
    }
  }
  i
}

fill_empty_header_cell= function(header_rows){
  # 合并的单元格，只有最左边的cell有数据，
  # 因此每行header，遇到NA，就填充其左侧数据
  # 这是暴力合并的方法，遇到负责格式可能有错
  for(i in 1:nrow(header_rows)){
    for(j in 2:ncol(header_rows))
      if(is.na(header_rows[i,j])){
        header_rows[i,j] = header_rows[i,j-1]
      }
  }
  header_rows
}

compact_header_rows = function(header_rows, sep = "_"){
  # 多行表头压缩成1行
  ret = sapply(header_rows,function(x){
    x = str_trim(x)
    x = x[x!=""]
    
    names = unique(x[!is.na(x)])
    paste(names, collapse = sep)
  })
  unname(ret)
}

convert_cnstats_by_xls = function(file_path){
  # 把统计年鉴的假冒xls，转换为tibble
  # 但是当作真的xls的来读取
  raw_table = read_excel(file_path)
  
  title = colnames(raw_table)[1]
  
  year = str_extract(title,"((19|20)\\d{2})")
  
  #head(raw_table)
  #tail(raw_table)
  # data_table = data.frame(raw_table[-c(1,2),])
  # head(data_table)
  
  df = raw_table %>% 
    filter(!grepl("单位",unlist(.[,1]))) %>% 
    remove_empty_last_row 
  
  first_data_row = find_first_data_row(df);first_data_row
  
  header_rows = df[1:first_data_row-1,]
  
  header_rows = fill_empty_header_cell(header_rows)
  
  df_data = df[first_data_row:nrow(df),]
  
  headers = compact_header_rows(header_rows)
  
  colnames(df_data) = headers
  
  df_data[,-1] = sapply(df_data[,-1],as.numeric)
  
  df_data[,1] = str_trim(unlist(df_data[,1]))
  
  structure(df_data,
            title = title,
            year=year)
}


convert_cnstats_by_html = function(file_path){
  # 把统计年鉴的假冒xls，转换为tibble
  # 但是当作html的来读取
  raw_html = read_file(file_path)
  
  
  title = raw_html %>%
    read_html() %>%
    html_node(xpath = '//*[@id="tti"]') %>%
    html_text()
  
  year = str_extract(title,"((19|20)\\d{2})")
  
  table_node = raw_html %>%
    read_html() %>%
    html_node("table.tb")
  
  df = table_node %>%
    html_table(fill = TRUE)
  
  data_n_rows = table_node %>% 
    html_nodes("td.zhibiao") %>% 
    length()
  
  header_n_rows = nrow(df) - data_n_rows
  
  first_data_row = header_n_rows + 1 #find_first_data_row(df);first_data_row
  
  header_rows = unspan_header(table_node, header_n_rows)
  
  df_data = df[first_data_row:nrow(df),]
  
  headers = compact_header_rows(header_rows)
  
  colnames(df_data) = headers
  
  df_data[,-1] = sapply(df_data[,-1],as.numeric)
  
  df_data[,1] = str_trim(unlist(df_data[,1]))
  
  structure(as.tibble(df_data),
            title = title,
            year=year)
}

