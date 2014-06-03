note
	description: "Summary description for {HTTP_404}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	HTTP_404 <p>

inherit
	HTTP_RESPONSE

create
	make

feature
	make (a_sock : separate <p> NETWORK_SOCKET_HANDLER ; a_filename : STRING)
		do
			make_sock (a_sock)
			filename := a_filename
		end

	send_response
		do
			put_line ("HTTP/1.1 404 FileNotFound")
			put_line ("")
			put_line ("404: Error, file ")
      put_line (filename)
      put_line (" not found.")
		end

	filename : STRING

end
