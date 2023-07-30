(module
  (type (;0;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;1;) (func))
  (import "wasi_snapshot_preview1" "fd_write" (func (;0;) (type 0)))
  (func (;1;) (type 1)
    i32.const 1028
    i32.const 0
    i32.store
    i32.const 1032
    i32.const 2
    i32.store
    i32.const 1
    i32.const 1028
    i32.const 2
    i32.const 1024
    call 0
    drop
  )
  (table (;0;) 0 0 funcref)
  (memory (;0;) 65536 65536)
  (export "memory" (memory 0))
  (export "__indirect_function_table" (table 0))
  (export "_start" (func 1))
  (data (;0;) (i32.const 0) "a\0a")
)