# Riverdelta

Riverdelta is a statically compiled language primarily targetting Webassembly.

Riverdelta files end in `.nil`.

## Items

A Riverdelta program consists of many items, like functions. Every item ends with a
semicolon.

```js
item = item_function | item_type | item_import | item_extern | item_mod
```

### Functions

A function is an item.

```js
function_sig  := IDENT "(" ( IDENT ":" type ),* ")" ( ":" type )?
item_function := "function" function_sig "=" expr ";"
```

```js
function helloWorld() = ;
```
```js
function block() = (
    1;
    2;
);
```

A function can have any amount of arguments and an optional return type annotation.
When no return type annotation is provided, the return type is `()`, the empty tuple.

### Types

Riverdelta has several different types.

`Int` is an unsigned 64 bit twos complement integer.

`I32` is an unsigned 32 bit twos complement integer.

`String` is an immutable string type.

`Bool` is a boolean (with the constants `true` and `false` being accessible in the global scope).

Struct types are user-defined types with several fields. They are accessible with the struct name like `MyStruct`. Structure types are all allocated on the heap and reference counted without cycle collection.

Raw pointers are unsafe types for implementing low level datastructures. They behave like structure types but do not have reference counting and no reference count header
in their representation. They are represented with an asterisk before a struct name,
like `*MyStruct`.

Tuples are an anonymous list of types. Unlike structs, tuples are always passed by-value. They are written using a list of types inside parentheses. Single element
tuples require a trailing comma.

The never type (`!`) is not constructible and the type of a loop without any breaks.

List types are a mutable homogenous list of values, allocated on the heap.
List types are written as the inner type inside brackets (like `[A]`).

Structure types are defined using the `type` item. There are also type aliases, which
are simply aliases for other types, with no extra semantics.

```
item-type := "type" IDENT "=" (struct-def | type) ";"

struct-def = "struct" "{" ( IDENT ":" type ),* "}"
```

```js
type Integer = Int;
type C = A;
type A = struct {
    a: Int,
    b: Int,
};
```

```js
type        := IDENT | type_rawptr | type_list | type_tuple | type_never
type_rawptr := "*" IDENT
type_list   := "[" type "]"
type_tuple  := "(" ( type ),* ")"
type_never  := "!"
```

### Imports

Imports allow importing functions from external WASM modules.

```js
item_import := "import" "(" literal_string literal_string ")" function_sig ";"
```

```js
import ("wasi_snapshot_preview1" "fd_write") fd_write(a: I32, b: I32, c: I32, d: I32): I32;
```

The two strings describe the module and function name used for the import, the identifier is the identifier used to call the function. If the signature does not match,the Webassembly module will produce validation errors at runtime.

### Externs

```js
item_extern = "extern" "mod" IDENT ";"
```

```js
extern mod hello;
```

Extern items instruct the compiler to load an external module in the search path with the name ident. If a module with that name has already been loaded, it is reused.
The compiler will implicitly load a module called `std`.

These external modules are parsed and checked separately, but codegened into the same Webassembly module as the main program. Extern items form a direct acylic graph. While it is possible to create cycles, the compiler will detect those and emit errors.

### Modules

Internal modules are declared using the `mod` item. These modules only exist for namespacing and file splitting. There are two kinds of modules, inline modules and file modules.

```js
item_mod := "mod" IDENT ( "(" ( item )* ")" )? ";"
```

```js
mod a (
    function a() = ;
);
mod b;
```

Inline modules, those that contain bodies, are declared in the same file. File modules, those without a body, are declared in a separate file. The file is relative to the directory of the file containing
the module statement.
`.nil` files cannot declare file submodules but only inline modules.
If the current file is `a/a.mod.nil`, then `mod foo;` will look for `a/b.nil` or `a/b/b.mod.nil`.


### Globals

Globals are mutable values.

```js
item_global := "global" IDENT ":" type "=" expr ";"
```

```js
global A: Int = 0;
```

They can be accessed like local variables. Their initial value must be literal expression.

## Expressions

there are many expressions and im not going to list a single one.