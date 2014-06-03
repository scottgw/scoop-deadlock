class PHILO <pef1, pef2>

feature
  left  : separate <pef1> FORK
  right : separate <pef2> FORK

  make (l : separate <pef1> FORK; r : separate <pef2> FORK)
    do
      left  := l
      right := r
    ensure-locks <pef1, pef2>
    end

  live
    do
      from until False loop
        step
      end
    ensure-locks <pef1, pef2>
    end

  step
    do
      think
      eat (left, right)
    ensure-locks <pef1, pef2>
    end

  think
    do
    end

  eat (l : separate <pef1> FORK; r : separate <pef2> FORK)
    do
    ensure-locks <pef1, pef2>
    end

end
