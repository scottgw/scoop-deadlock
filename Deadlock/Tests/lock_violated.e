class LOCK_VIOLATED <yp, zp>

feature
  y : separate <yp> LOCK_VIOLATED

  foo (x : separate <xp> LOCK_VIOLATED) <xp>
    require-locks
      < yp < dot >
    do
      bar (y)
    ensure-locks <yp>
    end

  bar ()
    do
    ensure-locks <zp>
    end
end
