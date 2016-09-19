-module(ts_config_dns).

-export([parse_config/2]).

-include("ts_dns.hrl").
-include_lib("tsung/include/ts_profile.hrl").
-include_lib("tsung/include/ts_config.hrl").

-include_lib("xmerl/include/xmerl.hrl").

parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
  ?LOGF("parse_config dyn_variable ~p~n", [Element], ?NOTICE),
  ts_config:parse(Element, Conf);
parse_config(Element = #xmlElement{name=dns, attributes=Attrs},
             Conf = #config{curid=Id, session_tab=Tab, sessions=[CurS | _],
                            dynvar=DynVar, subst=SubstFlag, match=MatchRegExp}) ->
  ?LOGF("parse_config dns ~p", [Element], ?NOTICE),
  Hostname = ts_config:getAttr(string, Attrs, hostname, none),
  Type = ts_config:getAttr(string, Attrs, type, "A"),
  Req = #dns_request{hostname=Hostname, type=Type},
  Msg = #ts_request{ack=parse,
                    endpage=true,
                    dynvar_specs=DynVar,
                    subst=SubstFlag,
                    match=MatchRegExp,
                    param=Req},
  ts_config:mark_prev_req(Id-1, Tab, CurS),
  ets:insert(Tab,{{CurS#session.id, Id}, Msg }),
  lists:foldl(fun ts_config:parse_config/2, Conf#config{dynvar=[]},
              Element#xmlElement.content);
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
  ts_config:parse(Element,Conf);
parse_config(_, Conf = #config{}) ->
  Conf.
