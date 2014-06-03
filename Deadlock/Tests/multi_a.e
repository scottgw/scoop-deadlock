class MULTI_A <p>

  feature
    make <p>
      require-locks
        < p < dot >
      local
        b : MULTI_B
        x : separate <p> MULTI_B
      do
        b.foo (x)
      ensure-locks
        <p>
      end
end
