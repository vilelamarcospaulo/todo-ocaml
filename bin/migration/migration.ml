let _ =
  let sysmap = Sysmap.init Sysmap.env_config in
  Todo.Migration.run sysmap.sqlconn
