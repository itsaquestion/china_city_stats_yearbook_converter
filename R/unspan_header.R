library(htmltools)
library(unpivotr)

unspan_header = function(table_node, header_n_rows){
  # 将html中的合并后的单元格（表头），还原成独立cells
  # 并且拷贝填充正确的值
  df = table_node %>%
    tidy_table()
  
  header_info = filter(df,row <= header_n_rows) %>% data.frame()
  
  header_df_raw = matrix(header_info$html,
                         nrow=max(header_info$row),
                         ncol=max(header_info$col)) %>% as.tibble()
  
  header_df_clear = header_df_raw
  
  for(i in 1:nrow(header_info)){
    row = header_info[i,]$row
    
    col = header_info[i,]$col
    
    if(!is.na(header_info[i,]$html)){
      
      html_obj = read_html(header_info[i,]$html) %>%
        html_node("td")
      
      rowspan = html_obj %>% 
        html_attr("rowspan", default = NA_character_) %>%
        as.numeric()
      
      colspan = html_obj %>% 
        html_attr("colspan", default = NA_character_) %>%
        as.numeric()
      
      text = html_text(html_obj)
      
      header_df_clear[row,col] = text 
      
      if(!is.na(rowspan)){
        for(j in (1:rowspan)-1){
          header_df_clear[row + j,col] = text 
        }
      }
      
      if(!is.na(colspan)){
        for(j in (1:colspan)-1){
          header_df_clear[row ,col+j] = text 
        }
      }
      
    }
    
  }
  
  header_df_clear
  
}




