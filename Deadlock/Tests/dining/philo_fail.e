class PHILO_FAIL <pef1, pef2>

feature
  left  : separate <pef1> FORK
  right : separate <pef2> FORK

  make (l : separate <pef1> FORK; r : separate <pef2> FORK)
    do
      left  := l
      right := r
    end

  live
    do
      from
      until False
      loop
        step
      end
    ensure-locks <pef1, pef2>
    end

  step
    do
      think
      eat_left (left)
    ensure-locks <pef1, pef2>
    end

  think
    do
    end

  eat_left (l : separate <pef1> FORK)
    do
      eat_right (right)
    ensure-locks <pef2>
    end

  eat_right (r : separate <pef2> FORK)
    do
    end

end
