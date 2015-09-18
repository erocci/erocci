%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% @doc Config has 2 sources:
%%%  - application env
%%%  - load/1 arg
%%% Application env override load/1 env
%%%
%%% Config properties:
%%%  - backends: a list of backends, loaded with occi_store:register/1
%%%  - name: base URI of the server. Normally, listeners are responsible for discovering name 
%%%          (e.g.: http://localhost:8080)
%%%  - listeners: a list of listeners, loaded with occi_listener:register/1.
%%%  - backend_timeout: timeout after which a backend is considered dead. Default to 5000 (ms)
%%%  - categories_map: a function for mapping category id (occi_cid()) to an URI. Default to 
%%%                    occi_category_mgr:hash/1
%%%  - categories_prefix: prefix for collections, for the occi_category_mgr:hash/1 function. 
%%%                       Default: /collections
%%%
%%% All other properties are stored in the config manager and accessible with get/1 and get/2.
%%% @end
%%% Created : 20 Jan 2014 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(occi_config).

-behaviour(gen_server).

-include("occi.hrl").

-export([start_link/0,
	 load/1,
	 get/1,
	 get/2,
	 set/2,
	 gen_id/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {tid}).

start_link() ->
    ?info("Starting OCCI config manager"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% @doc Config is a proplist, which can be overriden by application env
-spec load(list()) -> ok | {error, term()}.
load(Config) ->
    ?info("Loading erocci configuration~n", []),
    Env = application:get_all_env(erocci_core),
    try setup(Env ++ Config) of
	_ -> ok
    catch throw:Err -> {error, Err}
    end.
	     

get(Name) ->
    get(Name, undefined).

get(Name, Default) ->
    gen_server:call(?SERVER, {get, Name, Default}).

set(Name, Value) ->
    gen_server:call(?SERVER, {set, Name, Value}).


-spec gen_id(string() | binary(), occi_env()) -> uri().
gen_id(Prefix, #occi_env{req_uri=#uri{host=Host}}) ->
    occi_uri:gen_id(Prefix, Host).

%%%
%%% gen_server callbacks
%%%

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, term()} | {error, term()} | ignore.
init([]) ->
    case occi_table_mgr:new(?SERVER, [set, private, {keypos, 1}, {read_concurrency, true}]) of
	{ok, Tid} ->
	    {ok, #state{tid=Tid}};
	{error, Err} ->
	    {stop, {error, Err}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get, Name, Default}, _From, #state{tid=T}=State) ->
    case ets:lookup(T, Name) of
	[] ->
	    {reply, Default, State};
	[{_, Value}] ->
	    {reply, Value, State}
    end;

handle_call({set, Name, Value}, _From, #state{tid=T}=State) ->
    ets:insert(T, {Name, Value}),
    {reply, ok, State};

handle_call(Req, From, State) ->
    ?error("Unknown message from ~p: ~p~n", [From, Req]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{tid=T}) ->
    occi_table_mgr:delete(T).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Private
%%%
setup(Props) ->
    P2 = opt_categories_map(Props),
    P3 = opt_categories_prefix(P2),
    P4 = opt_backends(P3),
    P5 = opt_listeners(P4),
    P6 = opt_backend_timeout(P5),
    opt_store(P6).

%%%
%%% Option handlers
%%%
opt_categories_map(Props) ->
    case proplists:get_value(categories_map, Props) of
	undefined ->
	    set(categories_map, {occi_category_mgr, hash}),
	    Props;
	{Mod, Fun} ->
	    case erlang:function_exported(Mod, Fun, 1) of
		true -> 
		    set(categories_map, {Mod, Fun}),
		    proplists:delete(categories_map, Props);
		false -> 
		    throw({error, {invalid_conf, categories_map}})
	    end
    end.

opt_categories_prefix(Props) ->
    case proplists:get_value(categories_prefix, Props) of
	undefined -> Props;
	[$/ | Prefix ] ->
	    set(categories_prefix, [$/ | Prefix]),
	    proplists:delete(categories_prefix, Props);
	_ ->
	    throw({error, {invalid_conf, categories_prefix}})
    end.

opt_backends(Props) ->
    case proplists:get_value(backends, Props) of
	undefined -> Props;
	Backends ->
	    F = fun ([], _) ->
			ok;
		    ([B|Tail], Fun) ->
			case occi_store:register(B) of
			    {ok, _Pid} ->
				Fun(Tail, Fun);
			    {error, Err} ->
				throw({error, Err})
			end
		end,
	    F(Backends, F),
	    proplists:delete(backends, Props)
    end.

opt_listeners(Props) ->
    case proplists:get_value(listeners, Props) of
	undefined -> Props;
	Listeners -> 
	    F = fun ([], _) ->
			ok;
		    ([L|Tail], Fun) ->
			case occi_listener:register(L) of
			    {ok, _Pid} ->
				Fun(Tail, Fun);
			    {error, Err} ->
				{error, Err}
			end
		end,
	    case F(Listeners, F) of
		ok ->
		    proplists:delete(listeners, Props);
		{error, Err} ->
		    throw({error, Err})
	    end
    end.

opt_backend_timeout(Props) ->
    case proplists:get_value(backend_timeout, Props, 5000) of
	5000 -> 
	    set(backend_timeout, 5000);
	V when is_integer(V) ->
	    set(backend_timeout, V);
	V when is_list(V) ->
	    set(backend_timeout, list_to_integer(V))
    end,
    proplists:delete(backend_timeout, Props).

% All remaining options are stored in the table
opt_store(Props) ->
    lists:foreach(fun (Key) ->
			  set(Key, proplists:get_value(Key, Props))
		  end, proplists:get_keys(Props)).
