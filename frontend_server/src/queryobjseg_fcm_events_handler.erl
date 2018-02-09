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
init(ServerKey) ->
  {ok, ServerKey}.

handle_info(_Info, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  {ok, not_implemented, State}.

handle_event({send_message, RequestId, DeviceId, Base64Mat}, ServerKey) ->
  % Scope = <<"https://www.googleapis.com/auth/firebase.messaging">>,
  % OAuthEndpoint = <<"https://www.googleapis.com/oauth2/v4/token">>,
  URL = "https://fcm.googleapis.com/fcm/send",
  Body = #{ <<"request_id">> => RequestId
          , <<"device_id">> => DeviceId
          , <<"base64_mat">> => Base64Mat
          , <<"processed_at">> => timestamp()
          }
  % Now = timestamp(),
  % Request = #{ <<"iss">> => Email
  %            , <<"scope">> => Scope
  %            , <<"aud">> => OAuthEndpoint
  %            , <<"iat">> => Now
  %            , <<"exp">> => Now + 3600
  %            },
  % % _ = lager:info("Request: ~p", [Request]),
  % % _ = lager:info("PrivateKey: ~p", [PrivateKey]),
  % Jwt = binary_to_list(jwerl:sign(Request, rs16, PrivateKey)),

  % _ = lager:info("JWT: ~p", [Jwt]),
  Json = sr_json:encode(Body),
  Method = post,
  Header = [{"Authorization", string:concat("key=", ServerKey)}],
  Type = "application/json",
  % N = string:concat("grant_type=", "urn:ietf:params:oauth:grant-type:jwt-bearer"),
  % P = string:concat("assertion=", Jwt),
  % Body = string:join([N, P], "&"),
  HTTPOptions = [],
  Options = [],
  HTTPRequest = {URL, Header, Type, Json},
  _ = lager:info("Request Body ~p", [HTTPRequest]),
  {ok, Response} = httpc:request(Method, HTTPRequest, HTTPOptions, Options),
  lager:info("Response ~p", [Response]),
  % canillita_news_handler:notify(Entity),
  % _ = lager:info("Current state: ~p", [State]),
  {ok, ServerKey};
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
