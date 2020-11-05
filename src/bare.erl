-module(bare).

-export([encode/2,
         encode_iolist/2,
         decode/2]).

-type bigint() :: uint | int.
-type unsigned() :: u8 | u16 | u32 | u64.
-type signed() :: i8 | i16 | i32 | i64.
-type floats() :: f32 | f64.
-type numeric() :: bigint() | unsigned() | signed() | floats().

-type enum() :: {enum, [atom() | {atom(), non_neg_integer()}]}.

-type type() :: numeric() |
                bool |
                enum() |
                string() |
                {data, pos_integer()} | data |
                void |
                {optional, type()} |
                {array, type(), pos_integer()} |
                {array, type()} |
                {map, type(), type()} |
                {union, [type() | [type() | non_neg_integer()], ...]} |
                {struct, [{atom(), type()}, ...]}.
-type spec() :: term().

%% @doc Encode `Data' accordingly to `Spec' into binary.
%% @end
-spec encode(Data::term(), Spec::spec()) -> binary().
encode(Input, Type) -> iolist_to_binary(encode_iolist(Input, Type)).

%% @doc Encode `Data' accordingly to `Spec' into `iodata()'.
%% @end
-spec encode_iolist(Data::term(), Spec::spec()) -> iodata().
encode_iolist(Input, uint) when is_integer(Input), Input >= 0, Input < 16#80 ->
    <<Input>>;
encode_iolist(Input, uint) when is_integer(Input) ->
    [<<1:1, Input:7>> | encode_iolist(Input bsr 7, uint)];
encode_iolist(Input, int) when is_integer(Input), Input < 0 ->
    Neg = bnot Input,
    encode_iolist(2*Neg + 1, uint);
encode_iolist(Input, int) when is_integer(Input) ->
    encode_iolist(2*Input, uint);
%%% Unsigned integers
encode_iolist(Input, u8) when is_integer(Input), Input >= 0, Input =< 16#ff ->
    <<Input:8/unsigned-little-integer>>;
encode_iolist(Input, u16) when is_integer(Input), Input >= 0, Input =< 16#ffff ->
    <<Input:16/unsigned-little-integer>>;
encode_iolist(Input, u32) when is_integer(Input), Input >= 0, Input =< 16#ffffffff ->
    <<Input:32/unsigned-little-integer>>;
encode_iolist(Input, u64) when is_integer(Input), Input >= 0, Input =< 16#ffffffffffffffff ->
    <<Input:64/unsigned-little-integer>>;
%%% Signed integers
encode_iolist(Input, i8) when is_integer(Input), Input >= -16#80, Input =< 16#7f ->
    <<Input:8/signed-little-integer>>;
encode_iolist(Input, i16) when is_integer(Input), Input >= -16#8000, Input =< 16#7fff ->
    <<Input:16/signed-little-integer>>;
encode_iolist(Input, i32) when is_integer(Input), Input >= -16#80000000, Input =< 16#7fffffff ->
    <<Input:32/signed-little-integer>>;
encode_iolist(Input, i64) when is_integer(Input), Input >= -16#8000000000000000, Input =< 16#7fffffffffffffff ->
    <<Input:64/signed-little-integer>>;
%%% Floats
%%%% TODO: Should we add support for NaNs and infinities there?
encode_iolist(Input, f32) when is_float(Input) ->
    <<Input:32/float>>;
encode_iolist(Input, f64) when is_float(Input) ->
    <<Input:64/float>>;
%%% Booleans
encode_iolist(true, bool) -> <<1>>;
encode_iolist(false, bool) -> <<0>>;
%%% Binary
encode_iolist(Input, {data, Size}) when byte_size(Input) =:= Size ->
    Input;
encode_iolist(Input, data) when is_binary(Input) ->
    [encode_iolist(byte_size(Input), uint), Input];
%%% Strings
encode_iolist(Input, string) when is_list(Input) ->
    encode_iolist(list_to_binary(Input), data);
encode_iolist(Input, string) when is_binary(Input) ->
    encode_iolist(Input, data);
%%% Void
encode_iolist([], void) ->
    [];
%% Composite types
%%% Optional<type>
encode_iolist(undefined, {optional, _}) -> <<0>>;
encode_iolist(Input, {optional, Type}) ->
    Data = encode_iolist(Input, Type),
    [<<1>> | Data];
%%% [size]type
encode_iolist(Input, {array, Type, Size}) when length(Input) =:= Size ->
    [encode_iolist(Field, Type) || Field <- Input];
%%% []type
encode_iolist(Input, {array, Type}) ->
    Size = encode_iolist(length(Input), uint),
    Content = [encode_iolist(Field, Type) || Field <- Input],
    [Size, Content];
%%% map[key]value
encode_iolist(Input, {map, KeyType, ValueType}) when is_map(Input) ->
    Size = encode_iolist(map_size(Input), uint),
    Content = [[encode_iolist(Key, KeyType), encode_iolist(Value, ValueType)]
               || {Key, Value} <- maps:to_list(Input)],
    [Size, Content];
%%% (type1 | type2 | …)
encode_iolist({Type, Input}, {union, Types}) ->
    Expanded = expand(Types),
    case keyfind(Type, Expanded) of
        undefined ->
            error(no_matching_type, [Input, {union, Types}]);
        [Type | Id] ->
            [encode_iolist(Id, uint) | encode_iolist(Input, Type)]
    end;
%%% Struct
encode_iolist(Input, {struct, Fields}) when is_list(Fields), is_map(Input) ->
    [encode_iolist(map_get(Key, Input), Type) || {Key, Type} <- Fields].

%% @doc Decode `Input' for message defined by `Spec'.
%% @end
-spec decode(Input::binary(), Spec::spec()) -> {ok, term(), binary()} | {error, term()}.
%%% Unsized integers
%%%% Unsigned
decode(<<1:1, N:7, Rest0/binary>>, uint) ->
    case decode(Rest0, uint) of
        {ok, M, Rest} ->
            {ok, (M bsl 7) + N, Rest};
        Error ->
            Error
    end;
decode(<<N, Rest/binary>>, uint) ->
    {ok, N, Rest};
%%%% Signed
decode(Binary, int) ->
    case decode(Binary, uint) of
        {ok, N, Rest} when N rem 2 == 1 ->
            {ok, bnot ((N - 1) div 2), Rest};
        {ok, N, Rest} ->
            {ok, N div 2, Rest};
        Error ->
            Error
    end;
%%% Sized integers
decode(<<N:8/unsigned-little-integer, Rest/binary>>, u8) ->
    {ok, N, Rest};
decode(<<N:16/unsigned-little-integer, Rest/binary>>, u16) ->
    {ok, N, Rest};
decode(<<N:32/unsigned-little-integer, Rest/binary>>, u32) ->
    {ok, N, Rest};
decode(<<N:64/unsigned-little-integer, Rest/binary>>, u64) ->
    {ok, N, Rest};
decode(<<N:8/signed-little-integer, Rest/binary>>, i8) ->
    {ok, N, Rest};
decode(<<N:16/signed-little-integer, Rest/binary>>, i16) ->
    {ok, N, Rest};
decode(<<N:32/signed-little-integer, Rest/binary>>, i32) ->
    {ok, N, Rest};
decode(<<N:64/signed-little-integer, Rest/binary>>, i64) ->
    {ok, N, Rest};
%%% Floats
%%%% We need special handling for NaNs and infinities as Erlang do not support
%%%% them natively.
decode(<<0:1, 16#ff:8, 0:23, Rest/binary>>, f32) -> {ok, infinity, Rest};
decode(<<1:1, 16#ff:8, 0:23, Rest/binary>>, f32) -> {ok, neg_infinity, Rest};
decode(<<0:1, 16#ff:8, _:23, Rest/binary>>, f32) -> {ok, qnan, Rest};
decode(<<1:1, 16#ff:8, _:23, Rest/binary>>, f32) -> {ok, snan, Rest};
decode(<<F:32/float, Rest/binary>>, f32) -> {ok, F, Rest};
decode(<<0:1, 16#7ff:11, 0:52, Rest/binary>>, f64) -> {ok, infinity, Rest};
decode(<<1:1, 16#7ff:11, 0:52, Rest/binary>>, f64) -> {ok, neg_infinity, Rest};
decode(<<0:1, 16#7ff:11, _:52, Rest/binary>>, f64) -> {ok, qnan, Rest};
decode(<<1:1, 16#7ff:11, _:52, Rest/binary>>, f64) -> {ok, snan, Rest};
decode(<<F:64/float, Rest/binary>>, f64) -> {ok, F, Rest};
%%% Booleans
decode(<<0, Rest/binary>>, bool) -> {ok, false, Rest};
decode(<<_, Rest/binary>>, bool) -> {ok, true, Rest};
%%% Binaries
decode(Input, {data, Size}) ->
    case Input of
        <<Data:Size/binary, Rest/binary>> ->
            {ok, Data, Rest};
        _ ->
            {error, {not_enough_data, Input}}
    end;
decode(Input, data) ->
    case decode(Input, uint) of
        {ok, Size, Rest} ->
            decode(Rest, {data, Size});
        Error ->
            Error
    end;
%%% String
decode(Input, string) -> decode(Input, data);
%% Composite
%%% Optional<type>
decode(<<0, Rest/binary>>, {optional, _}) -> {ok, undefined, Rest};
decode(<<_, Rest/binary>>, {optional, Type}) -> decode(Rest, Type);
%%% [size]type
decode(Input, {array, Type, Size}) ->
    reduce_n(Input, fun(I) -> decode(I, Type) end, Size);
%%% []type
decode(Input, {array, Type}) ->
    case decode(Input, uint) of
        {ok, Size, Rest} ->
            decode(Rest, {array, Type, Size});
        Error ->
            Error
    end;
%%% map[key]type
decode(Input, {map, KeyType, ValueType}) ->
    DecodeEntry = fun(I) ->
                          case decode(I, KeyType) of
                              {ok, Key, Rest1} ->
                                  case decode(Rest1, ValueType) of
                                      {ok, Value, Rest} ->
                                          {ok, {Key, Value}, Rest};
                                      Error ->
                                          Error
                                  end;
                              Error ->
                                  Error
                          end
                  end,
    case decode(Input, uint) of
        {ok, Size, Rest0} ->
            case reduce_n(Rest0, DecodeEntry, Size) of
                {ok, List, Rest} ->
                    {ok, maps:from_list(List), Rest};
                Error ->
                    Error
            end
    end;
%%% Union
decode(Input, {union, Types}) ->
    Expanded = expand(Types),
    case decode(Input, uint) of
        {ok, Id, Rest} ->
            case valfind(Id, Expanded) of
                undefined ->
                    {error, {unknown_enum_type, Id, Expanded}};
                [Type | Id] ->
                    decode(Rest, Type)
            end;
        Error ->
            Error
    end;
%%% Struct
decode(Input, {struct, Fields}) ->
    DecodeField = fun(I, {Name, Type}) ->
                          case decode(I, Type) of
                              {ok, Value, Rest0} ->
                                  {ok, {Name, Value}, Rest0};
                              Error ->
                                  Error
                          end
                  end,
    case map_while(Input, DecodeField, Fields) of
        {ok, List, Rest} ->
            {ok, maps:from_list(List), Rest};
        Error ->
            Error
    end;
decode(Input, void) ->
    {ok, [], Input};
decode(<<>>, _) ->
    {error, unexpected_eof}.

map_while(Input, F, List) -> map_while(Input, F, [], List).
map_while(Rest, _, Agg, []) ->
    {ok, lists:reverse(Agg), Rest};
map_while(Input, F, Agg, [Field | Tail]) ->
    case F(Input, Field) of
        {ok, D, Rest} ->
            map_while(Rest, F, [D | Agg], Tail);
        Error ->
            Error
    end.

reduce_n(Input, F, N) ->
    reduce_n(Input, F, [], N).
reduce_n(Rest, _, Agg, 0) ->
    {ok, lists:reverse(Agg), Rest};
reduce_n(Input, F, Agg, Size) ->
    case F(Input) of
        {ok, D, Rest} ->
            reduce_n(Rest, F, [D | Agg], Size - 1);
        Error -> Error
    end.

expand(Types) -> expand(Types, 0).
expand([], _) -> [];
expand([[_ | N] = Type | Rest], C) when is_integer(N), N > C ->
    [Type | expand(Rest, N+1)];
expand([[_ | N] | _], C) ->
    error(invalid_enum_id, [N, C]);
expand([Type | Rest], N) -> [[Type | N] | expand(Rest, N+1)].

keyfind(_, []) -> undefined;
keyfind(Key, [[Key | _] = Entry | _]) -> Entry;
keyfind(Key, [_ | Rest]) -> keyfind(Key, Rest).

valfind(_, []) -> undefined;
valfind(Val, [[_ | Val] = Entry | _]) -> Entry;
valfind(Val, [_ | Rest]) -> valfind(Val, Rest).

% match(N, uint) when is_integer(N), N >= 0 -> true;
% match(N, int) when is_integer(N) -> true;
% match(N, u8) when is_integer(N) -> true;
% match(N, u16) when is_integer(N) -> true;
% match(N, u32) when is_integer(N) -> true;
% match(N, u64) when is_integer(N) -> true;
% match(N, i8) when is_integer(N) -> true;
% match(N, i16) when is_integer(N) -> true;
% match(N, i32) when is_integer(N) -> true;
% match(N, i64) when is_integer(N) -> true;
% match(F, f32) when is_float(F) -> true;
% match(F, f64) when is_float(F) -> true;
% match(B, bool) when B =:= true; B =:= false -> true;
% match(Atom, {enum, Values}) when is_atom(Atom) ->
%     proplists:is_defined(Atom, Values);
% match(Bin, {data, Size}) when byte_size(Bin) =:= Size -> true;
% match(Bin, data) when is_binary(Bin) -> true;
% match([], void) -> true;
% match(undefined, {optional, _}) -> true;
% match(D, {optional, Type}) -> match(D, Type);
% match(List, {array, Type, Size}) when length(List) =:= Size ->
%     match(List, {array, Type});
% match(List, {array, Type}) ->
%     lists:all(fun(D) -> match(D, Type) end, List);
% match(Map, {map, KeyType, ValueType}) ->
%     maps:filter(fun(K, V) -> match(K, KeyType) andalso match(V, ValueType) end,
%                 Map) =/= #{};
% match(Data, {union, Types}) ->
%     lists:any(fun({Type, _}) -> match(Data, Type);
%                  (Type) -> match(Data, Type)
%               end, Types);
% match(Map, {struct, Fields}) ->
%     lists:all(fun({Key, Type}) -> match(map_get(Key, Map), Type) end, Fields);
% match(_, _) -> false.
