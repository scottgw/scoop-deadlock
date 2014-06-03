class CALL

feature
  make <a>
    require-locks < a < dot >
    local
      x : separate <a> CALL
    do
      foo (x)
    ensure-locks <a>
    end

  foo (x : separate <xp> CALL) <xp>
    do
    -- ensure-locks <xp>
    end
end
