class SUB_LOCK <a> a : < dot

feature
  make
    local
      x : separate <a> SUB_LOCK
    do
      foo (x)
    ensure-locks <a>
    end

  foo (x : separate <xp> SUB_LOCK) <xp>
    require-locks < xp < a >
    do
      bar (x)
    ensure-locks <a>
    end

  bar (x : separate <xp> SUB_LOCK) <xp>
    do
    ensure-locks <xp>
    end

end
