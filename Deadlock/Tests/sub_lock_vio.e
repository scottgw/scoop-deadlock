class SUB_LOCK_VIO 

feature
  make <a>
    require-locks < a < dot >
    local
      x : separate <a> SUB_LOCK_VIO
    do
      foo (x)
    ensure-locks < a >
    end

  foo (x : separate <xp> SUB_LOCK_VIO) <xp>
    do
      bar (x)
    ensure-locks <xp>
    end

  bar (x : separate <xp> SUB_LOCK_VIO) <xp, a>
    do
    ensure-locks <xp, a>
    end

end
