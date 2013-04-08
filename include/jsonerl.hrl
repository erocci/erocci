-define(record_to_struct(RecordName, Record),
	 % we are zipping record's field names and corresponding values together
   % then we turn it into tuple resulting in *struct* - erlang's equivalent of json's *object*
	 {lists:zip(
	    lists:map(fun(F) -> list_to_binary(atom_to_list(F)) end, record_info(fields, RecordName)),
			lists:map(
        %% convention record's *undefined* value is represented as json's *null*
        fun(undefined) -> null;
           (E) -> E
        end,
				%% we are turning the record into list chopping its head (record name) off
        tl(tuple_to_list(Record))
      )
    )
  }
).

-define(struct_to_record(RecordName, Struct),
        begin
						{[{_, {NewStruct}}]} = Struct,
            % I use fun here in order to avoid possible variable collison by shaddowing them
						fun(PropList) ->
										RecordName:new(PropList)
						end(
							[{bcmvc_model_utils:to_ex_a(K), V} || {K, V} <- NewStruct]
						 )
				end
       ).

-define(record_to_json(RecordName, Record),
  % serialize erlang struct into json string
  jiffy:encode(?record_to_struct(RecordName, Record))
).

-define(json_to_record(RecordName, Json),
  % decode json text to erlang struct
  ?struct_to_record(RecordName, jiffy:decode(Json))
).
