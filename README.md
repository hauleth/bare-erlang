BARE
=====

Implementation of [BARE][] encoding in Erlang.

## Usage

```erlang
Spec = {struct, [{name, string}, {age, uint}]},

Binary = bare:encode(#{name => "hauleth", age => 28}, Spec),
#{name => <<"hauleth">>, age => 28} = bare:decode(Binary, Spec)
```

### Types

```
| BARE name         | Spec format                    | Erlang type                                      |
| :                 | :                              | :                                                |
| `uint`            | `uint`                         | `non_neg_integer()`                              |
| `int`             | `int`                          | `integer()`                                      |
| `u8`              | `u8`                           | `non_neg_integer()`                              |
| `u16`             | `u16`                          | `non_neg_integer()`                              |
| `u32`             | `u32`                          | `non_neg_integer()`                              |
| `u64`             | `u64`                          | `non_neg_integer()`                              |
| `i8`              | `i8`                           | `integer()`                                      |
| `i16`             | `i16`                          | `integer()`                                      |
| `i32`             | `i32`                          | `integer()`                                      |
| `i64`             | `i64`                          | `integer()`                                      |
| `f32`             | `f32`                          | float or `qnan`/`snan`/`infinity`/`neg_infinity` |
| `f64`             | `f64`                          | float or `qnan`/`snan`/`infinity`/`neg_infinity` |
| `string`          | `string`                       | `unicode:chardata()`                             |
| `data`            | `data`                         | `binary()`                                       |
| `data<N>`         | `{data, N}`                    | `binary()` with `byte_size(Bin) =:= N`           |
| `void`            | `void`                         | `[]` (empty list)                                |
| `enum`            | `{enum, [atom(), ...]`         | `atom()` that is listed in the spec              |
| `optional<type>`  | `{optional, Type}`             | `undefined` or one of the other types            |
| `[size]type`      | `{array, Type, Size}`          | list of `type` that has exactly `Size` elements  |
| `[]type`          | `{array, Type}`                | list of `type`                                   |
| `map[key]type`    | `{map, KeyType, ValueType}`    | map in form of `#{KeyType => ValueType}`         |
| `(type1 | type2)` | `{union, [Type1, Type2], ...}` | tuple in form of `{Type, Data}`                  |
| `struct`          | `{struct, [{Key, Type}, ...]}` | map                                              |
```

## TODO

- Implement compiler for BARE description files

## License

[Apache 2.0](LICENSE)

[BARE]: https://baremessages.org
