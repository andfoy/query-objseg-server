-module(queryobjseg_fcm_events_handler).

-behaviour(gen_event).

-export([ init/1
        , terminate/2
        , handle_info/2
        , handle_call/2
        , code_change/3
        , handle_event/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_event functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([PrivateKey, Email]) ->
  {ok, [PrivateKey, Email]}.

handle_info(_Info, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  {ok, not_implemented, State}.

handle_event(gen_token, [PrivateKey, Email]) ->
  Scope = <<"https://www.googleapis.com/auth/firebase.messaging">>,
  OAuthEndpoint = <<"https://www.googleapis.com/oauth2/v4/token">>,
  Now = timestamp(),
  Exp = Now + 3600,
  Request = #{ <<"iss">> => Email
             , <<"scope">> => Scope
             , <<"aud">> => OAuthEndpoint
             , <<"iat">> => Now
             , <<"exp">> => Exp
             },
  % Jwt = jwerl:sign(Request, rs256, PrivateKey),
  % _ = lager:info("JWT: ~p", [Jwt]),
  % canillita_news_handler:notify(Entity),
  % _ = lager:info("Current state: ~p", [State]),
  {ok, [PrivateKey, Email]};
handle_event(Event, State) ->
  _ = lager:info("Ignored event: ~p", [Event]),
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Arg, _State) ->
  ok.

timestamp() ->
  {Megasecs, Secs, _} = os:timestamp(),
  (Megasecs * 1000000) + Secs.
