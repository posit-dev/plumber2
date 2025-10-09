#* @redirect !get /redir/<x> /newdir/<x>
#* @redirect any /broken/* /temp/*
#* @redirect any /remote http://example.com
NULL

#* @forward /proxy/ http://127.0.0.1:9876
NULL
