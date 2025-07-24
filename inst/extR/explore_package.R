if (FALSE) {
  
  # mixsmsn::smsn.mix with option \code{family = "Skew.t"}, returns same `nu` (t distribution dof) for all components
  
  
  mixsmsn:::d.mixedSN
  mixsmsn:::p.mixedSN # not define
  mixsmsn:::q.mixedSN # not define
  
  
  mixsmsn:::dSN # mixsmsn:::d.mixedSN # skew normal
  sn::dsn
  mixsmsn:::pSN # not defined
  sn::psn
  mixsmsn:::qSN # not defined
  mixsmsn:::rSN # not defined
  
  mixsmsn:::dt.ls # mixsmsn:::d.mixedST # skew t
  mixsmsn:::pt.ls # not defined
  mixsmsn:::qt.ls # not defined
  mixsmsn:::rt.ls # not defined
  
  #mix.print(m)
  
  
  
} # mixsmsn