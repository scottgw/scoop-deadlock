class DECL_CYCLE <>

feature
  main <>
    procs
      a : top
      b : < a
      a : < b
    do
    end
end
