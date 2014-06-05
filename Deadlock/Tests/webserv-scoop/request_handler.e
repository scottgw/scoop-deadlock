note
	description: "Summary description for {REQUEST_HANDLER}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	REQUEST_HANDLER <p>

create
	make

feature
  make (a_sock : separate <p> NETWORK_SOCKET_HANDLER)
    do
      sock := a_sock
    end

	sock : separate <p> NETWORK_SOCKET_HANDLER
  local_sock : separate <p> NETWORK_SOCKET_HANDLER

  execute
		do
			from until False
			loop
            load_socket (sock)
				handle_request (local_sock)
            close_socket (local_sock)
			end
      ensure-locks <p>
		end

  load_socket (a_sock : separate <p> NETWORK_SOCKET_HANDLER)
    require
      has_accepted: a_sock.accepted /= Void
    do
      local_sock := a_sock.accepted
      a_sock.remove_accepted
    end

  close_socket (a_sock : separate <p> SOCKET_HANDLER)
    do
      a_sock.close
    end

	handle_request (a_sock : separate <p> NETWORK_SOCKET_HANDLER)
		local
			last    : STRING
			http_req : HTTP_REQUEST
		do
			create http_req.make

      --FIXME: stop time traveling! This line converts the string constant
      --into the request line from down below... how?!
      --io.put_string ("socket write: " + a_sock.is_open_write.out) 
      --io.new_line

      a_sock.read_line
      last := a_sock.last_string -- read_line (a_sock)
      
			http_req.add_method (last)

			from
        a_sock.read_line
        last := a_sock.last_string
			until
				last /= Void and then last.is_equal ("%R")
			loop
				http_req.add_field (last)

        a_sock.read_line
        last := a_sock.last_string
			end

			process_request (http_req)
		end

	read_line (a_sock : separate <p> NETWORK_SOCKET_HANDLER) : STRING
    local
      str : STRING
		do
				a_sock.read_line -- _until (10000) -- thread_aware --_thread_aware
				str := sock.last_string
        Result := str.twin
		end


	process_request (req : HTTP_REQUEST)
    require-locks < p < dot >
		do
			if req.method = req.get_id then
				respond_with_uri (req.method_uri)
			elseif req.method = req.head_id then

			end
		end

	respond_with_uri (filename : STRING)
    require-locks < p < dot >
		local
			resp      : HTTP_RESPONSE
			resp_200  : HTTP_200
			resp_404  : HTTP_404
			file      : PLAIN_TEXT_FILE
			full_file : STRING
			content   : STRING
			env       : OPERATING_ENVIRONMENT
		do
			create env
			create content.make_empty

			full_file := env.current_directory_name_representation.append (filename)

			create file.make (full_file)

			if file.exists and then file.is_directory then
				create file.make (full_file.append ("index.html"))
			end

			if file.exists and then not file.is_directory then
				create resp_200.make (local_sock, file, False)
        resp := resp_200
			else
				create resp_404.make (local_sock, filename)
        resp := resp_404
			end

			resp.send_response
    ensure-locks <p>
		end

end
