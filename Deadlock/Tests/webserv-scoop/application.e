class
	APPLICATION <p>

create
	make

feature {NONE} -- Initialization
	sock : separate <p> NETWORK_SOCKET_HANDLER
  
	max_handlers : INTEGER

	make
      require-locks
      < p < dot >
      local
         i : INTEGER
      do
         create sock
         from i := 1
         until i < 10
         loop
            setup_handler
            i := i + 1
         end
      ensure-locks <p>
      end


   setup_handler
      require-locks < p < dot >
      procs q : < dot
            p : < q
      local
         handler : separate <q> <p> REQUEST_HANDLER
      do
         create handler.make (sock)
         run (handler)
      ensure-locks <p>
      end
   
   
   close_socket (a_sock : separate <p> NETWORK_SOCKET_HANDLER)
      do
         sock.set_linger_off
         sock.close
      end

   run (h : separate <q> <p> REQUEST_HANDLER)
      require-locks
      < p < q >
      do
         h.execute
      end

end
