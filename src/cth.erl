-module(cth).

-include_lib("common_test/include/ct.hrl").

-export([
    %% HTTP related funs
    r_code/2,

    r200/1,
    r204/1,
    r400/1,
    r401/1,
    r403/1,
    r404/1,
    r409/1,

    get/2, get/3,
    post/3, post/4,
    delete/2,
    request/5,
    get_file/2, get_file/3,

    %% asserts and comparison funs
    assert/3,

    encode_params/1,
    atomize_keys/1
]).

r200(V) -> r_code(200, V).
r204(V) -> r_code(204, V).
r400(V) -> r_code(400, V).
r401(V) -> r_code(401, V).
r403(V) -> r_code(403, V).
r404(V) -> r_code(404, V).
r409(V) -> r_code(409, V).

get(Config, ShortUrlData) ->
    request(Config, get, ShortUrlData, default_headers(Config), <<>>).

get(Config, ShortUrlData, Headers) ->
    request(Config, get, ShortUrlData, Headers, <<>>).

post(Config, ShortUrlData, Body) ->
    post(Config, ShortUrlData, default_headers(Config), Body).

post(Config, ShortUrlData, Headers, Body) ->
    {Body2, Headers2} = try_encode(Body, Headers),
    request(Config, post, ShortUrlData, Headers2, Body2).

delete(Config, ShortUrlData) ->
    request(Config, delete, ShortUrlData, default_headers(Config), <<>>).

request(Config, Method, ShortUrlData, Headers, Payload) ->
    Url = expand_url(Config, ShortUrlData),
    Options = [],
    {ok, StatusCode, RespHeaders, ClientRef} =
        hackney:request(Method, Url, Headers, Payload, Options),
    {ok, RespBody} = hackney:body(ClientRef),
    {StatusCode, RespHeaders, try_decode(StatusCode, RespHeaders, RespBody)}.

get_file(Config, ShortUrlData) ->
    get_file(Config, [], ShortUrlData).
get_file(Config, Headers, ShortUrlData) ->
    {ShortUrl, Params} = encode_params(ShortUrlData),
    Host = ?config(host, Config),
    Port = ?config(http_port, Config),
    Url = iolist_to_binary(["http://", Host, ":", integer_to_list(Port), ShortUrl, "?", Params]),
    Options = [],
    Payload = <<>>,
    {ok, StatusCode, RespHeaders, ClientRef} =
        hackney:request(get, Url, Headers, Payload, Options),
    {StatusCode, RespHeaders, read_body_md5(ClientRef)}.

read_body_md5(Ref) ->
    HContext = crypto:hash_init(md5),
    {ok, HContext2} = read_body_md5_(Ref, HContext),
    MD5 = crypto:hash_final(HContext2),
    cth_utils:hstr(MD5).

read_body_md5_(Ref, HContext) ->
    case hackney:stream_body(Ref) of
        {ok, Data} ->
            read_body_md5_(Ref, crypto:hash_update(HContext, Data));
        done ->
            {ok, HContext};
        {error, Reason} ->
            {error, Reason}
    end.

r_code(ExpectedCode, {Code, _Headers, Body}) ->
    assert(Code =:= ExpectedCode,
        "expected code ~p\n"
        "got     : ~p\n"
        "body    : ~p", [ExpectedCode, Code, Body]),
    Body.

mfa(M, F, A) ->
    io_lib:format("~p:~p/~p", [M, F, A]).

stacktrace() ->
    St = try throw(42) catch 42 -> erlang:get_stacktrace() end,
    [
        io_lib:format("\t~-60s (~s:~p)~n", [mfa(M, F, Arithy), File, N])
        || {M, F, Arithy, [{file, File}, {line, N}]} <- tl(St)
    ].

%%stop(Msg) -> stop("~s", [Msg]).
stop(Fmt, Args) ->
    Reason =
        try
            io_lib:format(Fmt, Args)
        catch _:_ ->
            ct:pal("bad io_lib format, got: ~p <- ~p", [Fmt, Args]),
            "<malformed reason string, see above>"
    end,
    St0 = stacktrace(),
    [_, _, _ | St] = lists:reverse(St0),
    ct:pal("Error, reason: ~s\nat:\n~s", [Reason, lists:reverse(St)]),
    ct:fail(assertion_failed).

assert(true, _, _) -> ok;
assert(false, ErrorMessage, Args) -> stop(ErrorMessage, Args).

expand_url(Config, ShortUrlData) ->
    {ShortUrl, Params} = encode_params(ShortUrlData),
    Host = ?config(host, Config),
    Port = ?config(http_port, Config),
    Version = ?config(api_version, Config),
    iolist_to_binary(["http://", Host, ":", integer_to_list(Port), "/api/", Version, ShortUrl, "?", Params]).

encode_params({Url, Params}) ->
    {Url, hackney_url:qs(maps:to_list(Params))};
encode_params(Url) ->
    encode_params({Url, #{}}).

try_encode({multipart, _}=Body, Headers) ->
    {Body, Headers};
try_encode(Body, Headers) ->
    try
        {jiffy:encode(Body), [
            {"Content-Type", "application/json"} |
            Headers
        ]}
    catch _:E ->
        Stacktrace = erlang:get_stacktrace(),
        ct:pal(error,
            "failed to encode HTTP request body to json"
            "~nreason    : ~p"
            "~nterm      : ~p"
            "~nstacktrace: ~p", [E, Body, Stacktrace]),
        throw(request_encode_error)
    end.

try_decode(_, _, <<>>) -> <<>>;
try_decode(Code, Header, Body) ->
    try
        atomize_keys(jiffy:decode(Body, [return_maps]))
    catch _:E ->
        Stacktrace = erlang:get_stacktrace(),
        ct:pal(error,
           "failed to decode HTTP response body"
           "~nreason    : ~p"
           "~ncode      : ~p"
           "~nheader    : ~p"
           "~nbody      : ~p"
           "~nstacktrace: ~p", [E, Code, Header, Body, Stacktrace]),
        throw(response_decode_error)
    end.

atomize_keys(List) when is_list(List) ->
    lists:map(fun atomize_keys/1, List);
atomize_keys(Map) when is_map(Map) ->
    maps:fold(fun
        (K, V, Acc) -> maps:put(binary_to_atom(K, latin1), atomize_keys(V), Acc)
    end, #{}, Map);
atomize_keys(V) -> V.

default_headers(Config) ->
    Key = headers,
    case lists:keyfind(Key, 1, Config) of
        {Key, H} -> H;
        false -> []
    end.
