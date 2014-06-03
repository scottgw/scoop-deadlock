class LOCK_VIOLATED <yp>

feature
  y : separate <yp> LOCK_VIOLATED

  foo (x : separate <xp> LOCK_VIOLATED) <xp>
    require-locks
      < yp < dot >
    do
      bar (y)
    ensure-locks <xp>
    end

  bar (z : separate <zp> LOCK_VIOLATED) <zp> 
    do
    ensure-locks <zp>
    end
end
