%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2013, Jean Parpaillon
%%% @doc Mostly got from ejabberd_config
%%%
%%% @end
%%% Created : 25 Mar 2013 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(occi_config).
-author('jean.parpaillon@free.fr').

%% API
-export([start/0, load_file/1, get/2, get/3, set/3]).
-export([is_file_readable/1]).
-export([prepare_opt_val/4]).

-include("occi_common.hrl").
-include("occi_config.hrl").
-include_lib("kernel/include/file.hrl").

-define(CONFIG_PATH, <<"erocci.cfg">>).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    mnesia:create_table(config,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, config)}]),
    mnesia:add_table_copy(config, node(), ram_copies),
    Config = get_config_path(),
    load_file(Config),
    ok.

-type check_fun() :: fun((any()) -> any()) | {module(), atom()}.

-spec get(any(), check_fun()) -> any().
get(Opt, F) ->
    get(Opt, F, undefined).

-spec get(any(), check_fun(), any()) -> any().
get(Opt, F, Default) ->
    case ets:lookup(config, Opt) of
				[#config{value = Val}] ->
						prepare_opt_val(Opt, Val, F, Default);
				_ ->
            Default
    end.

set(Opt, Val, State) ->
		State#state{opts = [#config{key = Opt, value = Val} |
												State#state.opts]}.

%% @doc Get the filename of the erocci configuration file.
%% The filename can be specified with: erl -config "/path/to/erocci.cfg".
%% It can also be specified with the environtment variable EROCCI_CONFIG_PATH.
%% If not specified, the default value 'erocci.cfg' is assumed.
%% @spec () -> string()
get_config_path() ->
    case application:get_env(config) of
				{ok, Path} -> Path;
				undefined ->
						case os:getenv("EROCCI_CONFIG_PATH") of
								false ->
										?CONFIG_PATH;
								Path ->
		    Path
	    end
    end.

%% @doc Load the occi configuration file.
%% This function will crash if finds some error in the configuration file.
%% @spec (File::string()) -> ok
load_file(File) ->
    Terms = get_plain_terms_file(File),
    Res = lists:foldl(fun process_term/2, #state{}, Terms),
    set_opts(Res).

set_opts(State) ->
    Opts = lists:reverse(State#state.opts),
    F = fun() ->
								lists:foreach(fun(R) ->
																			mnesia:write(R)
															end, Opts)
				end,
    case mnesia:transaction(F) of
				{atomic, _} -> ok;
				{aborted,{no_exists,Table}} ->
						MnesiaDirectory = mnesia:system_info(directory),
						?ERROR_MSG("Error reading Mnesia database spool files:~n"
											 "The Mnesia database couldn't read the spool file for the table '~p'.~n"
											 "erocci needs read and write access in the directory:~n   ~s~n"
											 "Maybe the problem is a change in the computer hostname,~n"
											 "or a change in the Erlang node name, which is currently:~n   ~p~n",
											 [Table, MnesiaDirectory, node()]),
						exit("Error reading Mnesia database")
    end.

%% @doc Read an occi configuration file and return the terms.
%% Input is an absolute or relative path to an occi config file.
%% Returns a list of plain terms,
%% in which the options 'include_config_file' were parsed
%% and the terms in those files were included.
%% @spec(string()) -> [term()]
%% @spec(iolist()) -> [term()]
get_plain_terms_file(File) when is_binary(File) ->
    get_plain_terms_file(binary_to_list(File));
get_plain_terms_file(File1) ->
    File = get_absolute_path(File1),
		case file:consult(File) of
				{ok, Terms} ->
						strings_to_binary(Terms);
				{error, {LineNumber, erl_parse, _ParseMessage} = Reason} ->
						ExitText = describe_config_problem(File, Reason, LineNumber),
						exit_or_halt(ExitText);
				{error, Reason} ->
						ExitText = describe_config_problem(File, Reason),
						?ERROR_MSG(ExitText, []),
						exit_or_halt(ExitText)
		end.

%% @doc Convert configuration filename to absolute path.
%% Input is an absolute or relative path to an ejabberd configuration file.
%% And returns an absolute path to the configuration file.
%% @spec (string()) -> string()
get_absolute_path(File) ->
    case filename:pathtype(File) of
	absolute ->
	    File;
	relative ->
	    Config_path = get_config_path(),
	    Config_dir = filename:dirname(Config_path),
	    filename:absname_join(Config_dir, File)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Errors reading the config file

describe_config_problem(Filename, Reason) ->
    Text1 = lists:flatten("Problem loading occi config file " ++ Filename),
    Text2 = lists:flatten(" : " ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    ExitText.

describe_config_problem(Filename, Reason, LineNumber) ->
    Text1 = lists:flatten("Problem loading occi config file " ++ Filename),
    Text2 = lists:flatten(" approximately in the line "
			  ++ file:format_error(Reason)),
    ExitText = Text1 ++ Text2,
    Lines = get_config_lines(Filename, LineNumber, 10, 3),
    ?ERROR_MSG("The following lines from your configuration file might be"
							 " relevant to the error: ~n~s", [Lines]),
    ExitText.

get_config_lines(Filename, TargetNumber, PreContext, PostContext) ->
    {ok, Fd} = file:open(Filename, [read]),
    LNumbers = lists:seq(TargetNumber-PreContext, TargetNumber+PostContext),
    NextL = io:get_line(Fd, no_prompt),
    R = get_config_lines2(Fd, NextL, 1, LNumbers, []),
    file:close(Fd),
    R.

get_config_lines2(_Fd, eof, _CurrLine, _LNumbers, R) ->
    lists:reverse(R);
get_config_lines2(_Fd, _NewLine, _CurrLine, [], R) ->
    lists:reverse(R);
get_config_lines2(Fd, Data, CurrLine, [NextWanted | LNumbers], R) when is_list(Data) ->
    NextL = io:get_line(Fd, no_prompt),
    if
	CurrLine >= NextWanted ->
	    Line2 = [integer_to_list(CurrLine), ": " | Data],
	    get_config_lines2(Fd, NextL, CurrLine+1, LNumbers, [Line2 | R]);
	true ->
	    get_config_lines2(Fd, NextL, CurrLine+1, [NextWanted | LNumbers], R)
    end.

%% If occi isn't yet running in this node, then halt the node
exit_or_halt(ExitText) ->
    case [Vsn || {occi, _Desc, Vsn} <- application:which_applications()] of
				[] ->
						timer:sleep(1000),
						halt(string:substr(ExitText, 1, 199));
				[_] ->
						exit(ExitText)
    end.

strings_to_binary([]) ->
    [];
strings_to_binary(L) when is_list(L) ->
    case is_string(L) of
        true ->
            list_to_binary(L);
        false ->
            strings_to_binary1(L)
    end;
strings_to_binary(T) when is_tuple(T) ->
    list_to_tuple(strings_to_binary(tuple_to_list(T)));
strings_to_binary(X) ->
    X.

strings_to_binary1([El|L]) ->
    [strings_to_binary(El)|strings_to_binary1(L)];
strings_to_binary1([]) ->
    [];
strings_to_binary1(T) ->
    T.

is_string([C|T]) when (C >= 0) and (C =< 255) ->
    is_string(T);
is_string([]) ->
    true;
is_string(_) ->
    false.

binary_to_strings(B) when is_binary(B) ->
    binary_to_list(B);
binary_to_strings([H|T]) ->
    [binary_to_strings(H)|binary_to_strings(T)];
binary_to_strings(T) when is_tuple(T) ->
    list_to_tuple(binary_to_strings(tuple_to_list(T)));
binary_to_strings(T) ->
    T.

format_term(Bin) when is_binary(Bin) ->
    io_lib:format("\"~s\"", [Bin]);
format_term(S) when is_list(S), S /= [] ->
    case lists:all(fun(C) -> (C>=0) and (C=<255) end, S) of
        true ->
            io_lib:format("\"~s\"", [S]);
        false ->
            io_lib:format("~p", [binary_to_strings(S)])
    end;
format_term(T) ->
    io_lib:format("~p", [binary_to_strings(T)]).

-spec prepare_opt_val(any(), any(), check_fun(), any()) -> any().
prepare_opt_val(Opt, Val, F, Default) ->
    Res = case F of
              {Mod, Fun} ->
                  catch Mod:Fun(Val);
              _ ->
                  catch F(Val)
          end,
    case Res of
        {'EXIT', _} ->
            ?INFO_MSG("Configuration problem:~n"
                      "** Option: ~s~n"
                      "** Invalid value: ~s~n"
                      "** Using as fallback: ~s",
                      [format_term(Opt),
                       format_term(Val),
                       format_term(Default)]),
            Default;
        _ ->
            Res
    end.

%% @spec (Path::string()) -> true | false
is_file_readable(Path) ->
    case file:read_file_info(Path) of
				{ok, FileInfo} ->
						case {FileInfo#file_info.type, FileInfo#file_info.access} of
								{regular, read} -> true;
								{regular, read_write} -> true;
								_ -> false
						end;
				{error, _Reason} ->
						false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Process terms

process_term({Opt, Val}, State) ->
		set(Opt, Val, State).
