deferred class
  HTTP_RESPONSE <p>
  
inherit
  MIME_GUESSER
    rename make as make_mime end
    
feature
  send_response
    deferred
    ensure-locks <p>
    end
  
feature {HTTP_RESPONSE}
  make_sock (a_sock : separate <p> NETWORK_SOCKET_HANDLER)
    do
      create buffer.make_empty
      sock := a_sock
    end

  put (s : STRING)
    do
      buffer := buffer.append (s)
    end

  put_integer (i : INTEGER)
    do
      
    end

  put_line (s : STRING)
    do
      buffer := buffer.append (s.append (crlf))
    end

  send
    do
      send_sep (sock)
    ensure-locks <p>
    end
  
  send_sep (s : separate <p> NETWORK_SOCKET_HANDLER)
    do
      s.put_string (buffer)
    end
  
  buffer   : STRING

  http_ver : STRING = "HTTP/1.1"
  crlf     : STRING = "%R%N"
  sock     : separate <p> NETWORK_SOCKET_HANDLER

end
