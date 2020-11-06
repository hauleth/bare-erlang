-module(prop_bare).

-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").

-export([]).

unsigned(Bits) ->
    Max = (1 bsl Bits) - 1,
    integer(0, Max).

signed(Bits) ->
    UMax = (1 bsl Bits) - 1,
    Max = UMax bxor (1 bsl (Bits - 1)),
    Min = bnot Max,
    integer(Min, Max).

type() ->
    ?LAZY(oneof([
                 {void, exactly([])},
                 {uint, non_neg_integer()},
                 {u8, unsigned(8)},
                 {u16, unsigned(16)},
                 {u32, unsigned(32)},
                 {u64, unsigned(64)},
                 {int, integer()},
                 {i8, signed(8)},
                 {i16, signed(16)},
                 {i32, signed(32)},
                 {i64, signed(64)},
                 {bool, bool()},
                 {string, ?LET(S, string(), unicode:characters_to_binary(S))},
                 {data, binary()},
                 ?LET(Len, pos_integer(), {{data, Len}, binary(Len)}),
                 ?LET({Type, Value}, non_void_type(), {{optional, Type}, oneof([Value, exactly(undefined)])}),
                 ?LET({Type, Value}, non_void_type(), {{array, Type}, list(Value)}),
                 ?LET({{KeyT, Key}, {ValT, Val}}, {key_type(), non_void_type()}, {{map, KeyT, ValT}, map(Key, Val)})
                ])).

non_void_type() ->
    ?SUCHTHAT({Type, _}, type(), Type =/= void).

key_type() ->
    ?SUCHTHAT({Type, _}, non_void_type(),
              case Type of
                  {data, _} -> false;
                  data -> false;
                  _ -> true
              end).

is_unique(List) -> check_uniqueness(lists:sort(List)).

check_uniqueness([]) -> true;
check_uniqueness([X, X | _]) -> false;
check_uniqueness([_ | Rest]) -> check_uniqueness(Rest).

is_keyunique(N, List) ->
    check_uniqueness(N, lists:keysort(N, List)).

check_uniqueness(_, []) -> true;
check_uniqueness(N, [A, B | _]) when element(N, A) =:= element(N, B) -> false;
check_uniqueness(N, [_ | Rest]) -> check_uniqueness(N, Rest).

struct() -> struct(type()).
struct(Fields) ->
    UniqueList = ?LAZY(?SUCHTHAT(List, non_empty(list({atom(), Fields})), is_keyunique(1, List))),
    ?LET(List, UniqueList,
         begin
             Def = [{Field, Type} || {Field, {Type, _}} <- List],
             Val = [{Field, Value} || {Field, {_, Value}} <- List],
             {{struct, Def}, maps:from_list(Val)}
         end).

bunion() -> bunion(type()).
bunion(Fields) ->
    ?LET(List, ?SUCHTHAT(L, non_empty(list(Fields)), is_unique(L)),
         begin
             {Types, _Values} = lists:unzip(List),

             {{exactly(union), Types}, oneof(List)}
         end).

back_and_forth(Data, Type) ->
    bare:decode(bare:encode(Data, Type), Type).

prop_uint() ->
    ?FORALL(N, non_neg_integer(), back_and_forth(N, uint) =:= {ok, N, <<>>}).

prop_int() ->
    ?FORALL(N, integer(), back_and_forth(N, int) =:= {ok, N, <<>>}).

prop_u8() ->
    ?FORALL(N, unsigned(8), back_and_forth(N, u8) =:= {ok, N, <<>>}).

prop_u16() ->
    ?FORALL(N, unsigned(16), back_and_forth(N, u16) =:= {ok, N, <<>>}).

prop_u32() ->
    ?FORALL(N, unsigned(32), back_and_forth(N, u32) =:= {ok, N, <<>>}).

prop_u64() ->
    ?FORALL(N, unsigned(64), back_and_forth(N, u64) =:= {ok, N, <<>>}).

prop_i8() ->
    ?FORALL(N, signed(8), back_and_forth(N, i8) =:= {ok, N, <<>>}).

prop_i16() ->
    ?FORALL(N, signed(16), back_and_forth(N, i16) =:= {ok, N, <<>>}).

prop_i32() ->
    ?FORALL(N, signed(32), back_and_forth(N, i32) =:= {ok, N, <<>>}).

prop_i64() ->
    ?FORALL(N, signed(64), back_and_forth(N, i64) =:= {ok, N, <<>>}).

prop_f32() ->
    ?FORALL(F, ?LET(D, float(), begin
                                    <<F:32/float>> = <<D:32/float>>,
                                    F
                                end),
            back_and_forth(F, f32) =:= {ok, F, <<>>}).

prop_f64() ->
    ?FORALL(F, float(), back_and_forth(F, f64) =:= {ok, F, <<>>}).

prop_bool() ->
    ?FORALL(B, bool(), back_and_forth(B, bool) =:= {ok, B, <<>>}).

prop_string() ->
    ?FORALL(S, string(), back_and_forth(S, string) =:= {ok, unicode:characters_to_binary(S), <<>>}).

prop_bin_string() ->
    ?FORALL(BS, ?LET(S, string(), unicode:characters_to_binary(S)),
            back_and_forth(BS, string) =:= {ok, BS, <<>>}).

prop_fixed_data() ->
    ?FORALL({Len, B}, ?LET(N, pos_integer(), {N, binary(N)}),
            back_and_forth(B, {data, Len}) =:= {ok, B, <<>>}).

prop_data() ->
    ?FORALL(B, binary(), back_and_forth(B, data) =:= {ok, B, <<>>}).

prop_simple_enum() ->
    Values = ?LET(L, non_empty(list(atom())),
                  ?LET(E, oneof([exactly(A) || A <- L]), {E, L})),
    ?FORALL({Elem, List}, Values, back_and_forth(Elem, {enum, List}) =:= {ok, Elem, <<>>}).

%% TODO: test enums with changed Ids

prop_empty_optional() ->
    ?FORALL({Type, _}, type(), back_and_forth(undefined, {optional, Type}) =:= {ok, undefined, <<>>}).

prop_optional() ->
    ?FORALL({Type, Value}, type(), back_and_forth(Value, {optional, Type}) =:= {ok, Value, <<>>}).

prop_fixed_array() ->
?FORALL({Type, Len, List}, ?LET({{Type, Value}, Len}, {type(), pos_integer()},
                           {Type, Len, vector(Len, Value)}),
        back_and_forth(List, {array, Type, Len}) =:= {ok, List, <<>>}).

prop_array() ->
    ?FORALL({Type, List}, ?LET({Type, Value}, type(), {Type, list(Value)}),
            back_and_forth(List, {array, Type}) =:= {ok, List, <<>>}).

prop_struct() ->
    ?FORALL({Definition, Map}, struct(oneof([type(), struct()])),
            back_and_forth(Map, Definition) =:= {ok, Map, <<>>}).

prop_union() ->
    ?FORALL({Definition, Value}, bunion(oneof([type(), struct()])),
            back_and_forth(Value, Definition) =:= {ok, Value, <<>>}).
