class PHILO_MAIN 

feature
  make <p1, p2, f1, f2>
    require-locks
	    <f1 < p1, f1 < p2, f2 < p1, f2 < p2>
    local
      fork1 : separate <f1> FORK
      fork2 : separate <f2> FORK
      phil1 : separate <p1> <f1, f2> PHILO
      phil2 : separate <p2> <f1, f2> PHILO
    do
      create fork1
      create fork2
      create phil1.make (fork1, fork2)
      create phil2.make (fork2, fork1)

      dine (phil1, phil2)
    ensure-locks <p1, p2, f1, f2>
    end

  dine ( phil1 : separate <p1> <f1, f2> PHILO 
       ; phil2 : separate <p2> <f1, f2> PHILO) <p1, p2, f1 ,f2>
    require-locks <f1 < p1, f1 < p2, f2 < p1, f2 < p2>
    do
      phil1.live
      phil2.live
    ensure-locks <f1, f2>
    end
end
