class
  HTTP_200 <p>
  
inherit
  HTTP_RESPONSE
    redefine
      send_response
    end
  
create
  make
  
feature
  make (a_sock : separate <p> NETWORK_SOCKET_HANDLER ; a_uri : FILE; gzip_req : BOOLEAN)
    local
      ext : STRING
    do
      make_sock (a_sock)
      
      create content.make_empty
      mime_type := Void
            
      if gzip_req then
--        io.put_string ("Unhandled gzip request%N")
      else
        a_uri.open_read
        a_uri.read_stream (a_uri.count)
        
        ext := a_uri.name
        ext := ext.substring (ext.last_index_of ('.', ext.count) + 1,
                              ext.count)
        mime_type := mime_table.item (ext) -- FIXME: using .item breaks SCOOP
        
        if mime_type = Void then
          mime_type := "text/html"
        end
        
        content := a_uri.last_string
      end
    -- ensure-locks <p>
    end
  
  redirect_to_content (s : STRING)
    do
      content := content.append (s)
    end
  
  content : STRING
  
  mime_type : STRING
  
  send_response
    do
      put (http_ver)
      put_line (" 200 OK")
      put_line ("Date: Thu, 09 Jul 2009 10:00:00 GMT")
      put ("Content-Type: ")
      put_line (mime_type)
      put_line ("Accept-Ranges: none")
      put_line ("Connection: close")
      put ("Content-Length: ")
      put_integer (content.count)
      put_line ("")
      put_line ("")
      buffer := buffer.append (content)
      
      send
    ensure-locks <p>
    end
end

