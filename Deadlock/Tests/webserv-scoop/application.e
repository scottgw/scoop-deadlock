class
	APPLICATION <p,q>

create
	make

feature {NONE} -- Initialization
	sock : separate <p> NETWORK_SOCKET_HANDLER
  
	max_handlers : INTEGER

	make
    require-locks
      <
      q < dot,
      p < q
      >
    local
      failed : BOOLEAN
      i      : INTEGER
      handler : separate <q> <p> REQUEST_HANDLER
    do
      create sock
      create handler.make (sock)
      run (handler)
    ensure-locks
      <q>
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
