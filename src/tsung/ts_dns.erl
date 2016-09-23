-module(ts_dns).

-export([
         add_dynparams/4
         ,decode_buffer/2
         ,dump/2
         ,get_message/2
         ,new_session/0
         ,parse/2
         ,parse_bidi/2
         ,parse_config/2
         ,session_defaults/0
       ]).

-behavior(ts_plugin).

-include("ts_dns.hrl").

-include_lib("tsung/include/ts_profile.hrl").
-include_lib("tsung/include/ts_macros.hrl").
-include_lib("kernel/src/inet_dns.hrl").

-define(RESPONSE_TIMEOUT, 15000).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session (ack_type and persistent)
%% Returns: {ok, true|false}
%%----------------------------------------------------------------------
session_defaults() ->
  ?LOG("session defaults", ?NOTICE),
  {ok, true}.


%%----------------------------------------------------------------------
%% Function: decode_buffer/2
%% Purpose: We need to decode buffer (remove chunks, decompress ...) for
%%          matching or dyn_variables
%% Returns: binary
%%----------------------------------------------------------------------
decode_buffer(Buffer, #dns_request{}) ->
  Buffer.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: #dns_request{} or []
%%----------------------------------------------------------------------
new_session() ->
  ?LOG("new_session", ?NOTICE),
  #dns_request{type="A"}.


%%----------------------------------------------------------------------
%% Function: get_message/2
%% Purpose: Build a message/request
%% Args:  #dns_request{}, #state_rcv{}
%% Returns: binary
%%----------------------------------------------------------------------
get_message(Req = #dns_request{type=TypeStr, hostname=Hostname},
            _State = #state_rcv{session=_Sess}) ->
  ?LOGF("Sending DNS request for ~p ~p by ~p", [TypeStr, Hostname, self()], ?INFO),
  TypeAtom = erlang:list_to_atom(string:to_lower(TypeStr)),
  Bin = inet_dns:encode(#dns_rec{header=#dns_header{rd=1},
                                 qdlist=[#dns_query{domain=Hostname,
                                                    type=TypeAtom, class=in}]}),
  timer:exit_after(?RESPONSE_TIMEOUT, timeout),
  {Bin, Req};
get_message(Req, State) ->
  ?LOGF("Got message ~p State ~p~n", [Req, State], ?DEB),
  ok.

%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: parse the response from the server and keep information
%%          about the response in State#state_rcv.session
%% Args:  Data (binary), State (#state_rcv)
%% Returns: {NewState, Options for socket (list), Close = true|false}
%%----------------------------------------------------------------------
parse(closed, State) ->
  ?LOG("Socket closed", ?DEB),
  {State#state_rcv{ack_done = true, datasize=0}, [], true};
parse(Data, State=#state_rcv{session=#dns_request{hostname=Hostname, type=Type},
                             acc = [], datasize=0}) ->
  ?LOGF("Got data from resolver ~w~n", [Data], ?DEB),
  case inet_dns:decode(Data) of
    {ok, DNS = #dns_rec{arlist=[], anlist=[]}} ->
      ?LOGF("Empty DNS resolve ~p", [DNS], ?INFO),
      ts_mon:add({count, no_answer});
    {ok, #dns_rec{anlist=AnList, arlist=ArList}} ->
      ?LOGF("Resolved ~p, ~p from ~p ~p", [AnList, ArList, Type, Hostname], ?DEB);
    Rest ->
      ?LOGF("DNS Decode bad return ~p from ~p ~p", [Rest, Type, Hostname], ?ERR)
  end,
  {State#state_rcv{ack_done=true, datasize = size(Data)}, [], true};
parse(Data, State) ->
  ?LOGF("Unmatched response ~p ~p~n", [Data, State], ?ERR),
  {State#state_rcv{ack_done=true, datasize = size(Data)}, [], true}.

parse_bidi(Data, State) ->
  ?LOGF("in parse bidi ~p ~p", [Data, State], ?NOTICE),
  ts_plugin:parse_bidi(Data, State).

%%----------------------------------------------------------------------
%% Function: parse_config/2
%% Purpose:  parse tags in the XML config file related to the protocol
%% Returns:  List
%%----------------------------------------------------------------------
parse_config(Element, Conf) ->
  ?LOG("parse_config", ?DEB),
	ts_config_dns:parse_config(Element, Conf).
 

-spec dump(protocol, {Request::#ts_request{},Session::term(), Id::integer(),
                      Host::string(),DataSize::integer()}) -> ok.
%%----------------------------------------------------------------------
%% Function: dump/2
%% Purpose:  log request and response summary
%% Returns:  ok.
%%----------------------------------------------------------------------
dump(A, B) ->
  % ?LOGF("dump ~p ~p~n", [A, B], ?DEB),
  ts_plugin:dump(A, B).


%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%%----------------------------------------------------------------------
add_dynparams(_Subst, DynData, Param, _Host) ->
  ?LOGF("add_dynparams Param: ~p Data ~p~n", [Param, DynData], ?DEB),
  Param.
