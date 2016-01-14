-module(espandsp).

%% API exports
-export([
    output_encoding/0,
    g711_open/1,
    g711_encode/2,
    g711_decode/2,
    g711_transcode/2
]).
-on_load(init/0).

-define(MAX_SIZE, 16384). %% 16KiB if we want more we need to enable dirty schedulers

-opaque g711_ctx() :: binary().
-type g711_ulaw()  :: binary().
-type g711_alaw()  :: binary().
-type g711()       :: g711_ulaw() | g711_alaw().

-type pcm_s16le()  :: binary().
-type pcm_s16be()  :: binary().
-type pcm_s16()    :: pcm_s16le() | pcm_s16be().

-export_type([
    g711_ctx/0,
    g711_ulaw/0,
    g711_alaw/0,
    g711/0
]).

-export_type([
    pcm_s16le/0,
    pcm_s16be/0,
    pcm_s16/0
]).

-define(APPNAME, espandsp).
-define(LIBNAME, espandsp).

%%====================================================================
%% API functions
%%====================================================================

-spec(g711_open(ulaw | alaw) -> g711_ctx()).
g711_open(Mode) when (Mode =:= alaw) or (Mode =:= ulaw) ->
    g711_open_nif(Mode).

-spec(g711_encode(g711_ctx(), pcm_s16()) -> g711()).
g711_encode(Ref, Bin) when is_binary(Bin),
    byte_size(Bin) rem 2 =:= 0, byte_size(Bin) =< ?MAX_SIZE ->
    g711_encode_nif(Ref, Bin).

-spec(g711_decode(g711_ctx(), g711()) -> pcm_s16()).
g711_decode(Ref, Bin) when is_binary(Bin), byte_size(Bin) =< ?MAX_SIZE ->
    g711_decode_nif(Ref, Bin).

-spec(g711_transcode(g711_ctx(), g711()) -> g711()).
g711_transcode(Ref, Bin) when is_binary(Bin), byte_size(Bin) =< ?MAX_SIZE ->
    g711_transcode_nif(Ref, Bin).

g711_open_nif(_) ->
    not_loaded(?LINE).

g711_encode_nif(_, _) ->
    not_loaded(?LINE).

g711_decode_nif(_, _) ->
    not_loaded(?LINE).

g711_transcode_nif(_, _) ->
    not_loaded(?LINE).

-spec(output_encoding() -> pcm_s16le | pcm_s16be).
output_encoding() ->
    output_encoding_nif().

output_encoding_nif() ->
    not_loaded(?LINE).

%%====================================================================
%% Internal functions
%%====================================================================
init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).


