%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 11 Mar 2015 by Jean Parpaillon <jean.parpaillon@free.fr>
-module(occi_backend_fs).

-behaviour(occi_backend).

-include("occi.hrl").
-include("occi_log.hrl").
-include_lib("statfs/include/statfs.hrl").
-include_lib("kernel/include/file.hrl").

-define(cid_ctnr, #occi_cid{scheme = <<"http://schemas.ogf.org/occi/storage#">>, 
			    term = <<"container">>, class=kind}).
-define(cid_ctnr_entry, #occi_cid{scheme = <<"http://schemas.ogf.org/occi/storage#">>, 
				  term = <<"containerentry">>, class=kind}).
-define(cid_data, #occi_cid{scheme = <<"http://schemas.ogf.org/occi/storage#">>, 
			    term = <<"data">>, class=kind}).
-define(cid_tb, #occi_cid{scheme = <<"http://schemas.ogf.org/occi/storage#">>, 
			  term = <<"thumbnail">>, class=kind}).

-define(nid, "occi").
-define(occibase, "meta").
-define(rawbase, "raw").
-define(TBL, ?MODULE).

-define(root_uri, #uri{path=[]}).
-define(root_node, occi_node:new(?root_uri, 
				 occi_collection:new(?root_uri, 
						     [#uri{path=?occibase}, 
						      #uri{path=?rawbase}]))).

-define(meta_uri, #uri{path=?occibase}).
-define(raw_uri, #uri{path=?rawbase}).

%% occi_backend callbacks
-export([init/1,
	 terminate/1]).
-export([update/2,
	 save/2,
	 delete/2,
	 find/2,
	 load/3,
	 action/3]).

-record(state, {mountpoint     :: uri(),
		basedir        :: string(),
		rawbase        :: uri()         % data content base uri
	       }).

-define(schema, filename:join([priv_dir(), "occi-storage.xml"])).

%%%===================================================================
%%% occi_backend callbacks
%%%===================================================================
init(#occi_backend{mountpoint=Mp, opts=Opts}) ->
    case proplists:get_value(basedir, Opts) of
	undefined ->
	    {error, {missing_opt, basedir}};
	Basedir when is_list(Basedir) ->
	    case file:read_file_info(Basedir) of
		{ok, #file_info{type=directory}} ->
		    init_mimetypes(Opts, #state{mountpoint=Mp, basedir=Basedir});
		{error, Err} ->
		    {error, Err}
	    end;
	_ ->
	    {error, {bad_opt, dir}}
    end.

terminate(#state{}) ->
    ok.

save(State, #occi_node{}=_Node) ->
    {ok, State}.

delete(State, #occi_node{}=_Node) ->
    {ok, State}.

update(State, #occi_node{}=_Node) ->
    {ok, State}.

find(State, #occi_node{type=capabilities, objid='_'}) ->
    {{ok, [occi_capabilities:new()]}, State};

find(State, #occi_node{id=?root_uri}) ->
    {{ok, [?root_node]}, State};

find(State, #occi_node{id=?raw_uri}) ->
    {{ok, [occi_node:new(?raw_uri, occi_collection:new(?raw_uri))]}, State};

find(#state{basedir=Basedir}=State, #occi_node{id=Uri}) ->
    Uri2 = occi_uri:rm_prefix(Uri, ?occibase),
    Fullpath = filename:nativename(filename:join(Basedir, Uri2#uri.path)),
    case file:read_file_info(Fullpath, [{time, posix}]) of
	{error, enoent} ->
	    {{ok, []}, State};
	{ok, #file_info{type=directory}} ->
	    {create_ctnr_node(Uri2, State), State};
	{ok, #file_info{type=regular}} ->
	    {create_data_node(Uri2, State), State};
	{ok, _} ->
	    {{ok, []}, State};
	{error, Err} ->
	    {{error, Err}, State}
    end;

find(State, #occi_node{}) ->
    {{ok, []}, State}.

load(State, #occi_node{type=occi_collection, objid=?cid_ctnr}=Node, _Opts) ->
    Coll = occi_collection:new(?cid_ctnr, [#uri{path=[]}]),
    {{ok, Node#occi_node{data=Coll}}, State};

load(State, #occi_node{}=Req, _Opts) ->
    {{ok, Req}, State}.

action(State, #uri{}, #occi_action{}) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_mimetypes(Opts, State) ->
    Path = proplists:get_value(mimetypes, Opts, "/etc/mime.types"),
    ?TBL = ets:new(?TBL, [set, public, named_table]),
    case httpd_conf:load_mime_types(Path) of
	{ok, Props} ->
	    ets:insert(?TBL, Props),
	    %init_raw(Opts, State);
	    {ok, [{schemas, [{path, ?schema}]}], State};
	{error, Err} ->
	    {error, Err}
    end.

%% init_raw(Opts, #state{mountpoint=#uri{path=Prefix}}=State) ->
%%     Rawbase = proplists:get_value(rawbase, Opts),
%%     case Rawbase of
%% 	#uri{scheme=http} ->
%% 	    #uri{path=Path}=Uri = occi_uri:add_prefix(Rawbase, Prefix),
%% 	    occi_http_common:add_route(http, {list_to_binary(Path), lz_fs_raw, []}),
%% 	    {ok, State#state{rawbase=Uri}};
%% 	_ ->
%% 	    lager:error("Incorrect rawbase: ~p~n", Rawbase),
%% 	    {error, invalid_rawbase}
%%     end.

create_ctnr_node(#uri{}=Uri, State) ->
    Uri2 = occi_uri:add_prefix(Uri, ?occibase),
    {ok, Kind} = occi_category_mgr:get(?cid_ctnr),
    Res = occi_resource:new(Uri2, Kind, [], []),
    case get_ctnr_children(Uri, State) of
	{error, Err} ->
	    {error, Err};
	Links ->
	    Res3 = occi_resource:set_attr_value(Res, <<"occi.storage.size">>, length(Links)),
	    Res4 = lists:foldl(fun (#occi_link{}=Link, Acc) ->
				       occi_resource:add_link(Acc, Link)
			       end, Res3, Links),
	    Node = occi_node:new(Res4, anonymous),
	    {ok, [Node#occi_node{objid=Uri2}]}
    end.

get_ctnr_children(#uri{path=Path}, #state{basedir=Prefix}) ->
    Fullpath = filename:nativename(filename:join(Prefix, Path)),
    case file:list_dir(Fullpath) of
	{ok, Filenames} ->
	    {ok, Kind} = occi_category_mgr:get(?cid_ctnr_entry),
	    lists:map(fun (Filename) ->
			      LinkId = occi_uri:gen_urn(?nid, filename:join(Fullpath, Filename)),
			      Target = case Path of
					   [] -> #uri{path=Filename};
					   _ -> #uri{path=filename:join(Path, Filename)}
				       end,
			      occi_link:new(LinkId, Kind, [], [], occi_uri:add_prefix(Target, ?occibase))
		      end, Filenames);
	{error, Err} ->
	    {error, Err}
    end.

create_data_node(#uri{path=Path}=Uri, #state{basedir=Prefix}=_State) ->
    Fullpath = filename:nativename(filename:join(Prefix, Path)),
    Attrs = [{<<"occi.storage.mimetype">>, get_mimetype(Fullpath)},
	     {<<"occi.storage.size">>, filelib:file_size(Fullpath)}],
    {ok, Kind} = occi_category_mgr:get(?cid_data),
    Uri2 = occi_uri:add_prefix(Uri, ?occibase),
    Node = occi_node:new(occi_resource:new(Uri2, Kind, [], Attrs), anonymous),
    {ok, [Node#occi_node{objid=Uri2}]}.

get_mimetype(Path) ->
    case filename:extension(Path) of
	[] -> <<"application/data">>;
	"." ++ Ext ->
	    case ets:lookup(?TBL, Ext) of
		[{Ext, Mime}] -> Mime;
		[] -> <<"application/data">>
	    end
    end.

priv_dir() ->
    case code:priv_dir(occi_backend_admin) of
	{error, bad_name} ->
	    filename:join([filename:dirname(code:which(?MODULE)),
			   "..",
			   "priv"]);
	Path ->
	    Path
    end.
