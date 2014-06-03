class NETWORK_SOCKET_HANDLER

inherit
  SOCKET_HANDLER

create
  make

feature
  make
    do
    end

  accept
    do
    end

  remove_accepted
    do
    end

  accepted : separate <dot> NETWORK_SOCKET_HANDLER
    do
    end

  ready_for_reading : BOOLEAN
    do
    end

  set_accept_timeout (i : INTEGER)
    do
    end

  read_line_until (i : INTEGER)
    do
    end

  listen (n : INTEGER)
    do
    end

  timeout : INTEGER
    do
    end

  set_linger_off
    do
    end

end
