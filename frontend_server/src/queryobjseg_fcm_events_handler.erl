-module(queryobjseg_fcm_events_handler).

-behaviour(gen_event).

-export([ init/1
        , terminate/2
        , handle_info/2
        , handle_call/2
        , code_change/3
        , handle_event/2
        ]).

-define(INTERVAL, 3600000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_event functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init({ServerKey, ServiceJson}) ->
  {ok, {ServerKey, ServiceJson, <<"no-token">>}}.

handle_info(_Info, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  {ok, not_implemented, State}.

handle_event(gen_token, {ServerKey, ServiceJson, _}) ->
  Endpoint = "https://www.googleapis.com/oauth2/v4/token",
  PrivateKey = jose_jwk:from_pem(maps:get(<<"private_key">>, ServiceJson)),
  Payload = #{ <<"iss">> => maps:get(<<"client_email">>, ServiceJson)
             , <<"scope">> => <<"https://www.googleapis.com/auth/datastore">>
             , <<"aud">> => <<"https://www.googleapis.com/oauth2/v4/token">>
             , <<"exp">> => os:system_time(seconds) + 3600
             , <<"iat">> => os:system_time(seconds)
             },
  Json = sr_json:encode(Payload),
  Signed = jose_jwt:sign(PrivateKey, #{ <<"alg">> => <<"RS256">> }, Payload),
  lager:info("JWT signature ~p", [Signed]),
  {_JWS, Token} = jose_jws:compact(Signed),
  lager:info("JWT Token ~p", [Token]),
  N = string:concat("grant_type=", "urn:ietf:params:oauth:grant-type:jwt-bearer"),
  P = string:concat("assertion=", binary_to_list(Token)),
  Body = string:join([N, P], "&"),
  Method = post,
  Type = "application/x-www-form-urlencoded",
  HTTPOptions = [],
  Options = [],
  Header = [],
  HTTPRequest = {Endpoint, Header, Type, Body},
  lager:info("HTTP Request ~p", [HTTPRequest]),
  {ok, {_, _, RespBody}} = httpc:request(Method, HTTPRequest, HTTPOptions, Options),
  lager:info("Response: ~p", [RespBody]),
  AuthToken = sr_json:decode(RespBody),
  % erlang:send_after(?INTERVAL, self(), gen_token),
  {ok, {ServerKey, ServiceJson, AuthToken}};
handle_event({send_message, Response, FirebaseToken}, {ServerKey, ServiceJson, AuthToken}) ->
  % Scope = <<"https://www.googleapis.com/auth/firebase.messaging">>,
  % OAuthEndpoint = <<"https://www.googleapis.com/oauth2/v4/token">>,
  FirestoreFmtURL = "https://firestore.googleapis.com/v1beta1/projects/~s/databases/(default)/documents/devices/~s/segmentations?documentId=~s",
  FirestoreURL = io_lib:format(
    FirestoreFmtURL, [binary_to_list(maps:get(<<"project_id">>, ServiceJson)),
                      binary_to_list(maps:get(<<"device_id">>, Response)),
                      binary_to_list(maps:get(<<"id">>, Response))]),
  lager:info("Firestore Endpoint: ~s", [FirestoreURL]),
  Header1 = [{"Authorization",
             string:concat("Bearer ",
                           binary_to_list(
                            maps:get(<<"access_token">>, AuthToken)))}],
  Method1 = post,
  Type1 = "application/json",
  FirestoreBody = #{ <<"fields">> => #{ <<"id">> => #{<<"stringValue">> => maps:get(<<"id">>, Response)}
                                      , <<"server">> => #{<<"stringValue">> => maps:get(<<"server">>, Response)}
                                      , <<"device_id">> => #{<<"stringValue">> => maps:get(<<"device_id">>, Response)}
                                      , <<"processed_at">> => #{<<"integerValue">> => maps:get(<<"processed_at">>, Response)}
                                      , <<"mask">> => #{<<"stringValue">> => maps:get(<<"mask">>, Response)}
                                      , <<"phrase">> => #{<<"stringValue">> => maps:get(<<"phrase">>, Response)}
                                      , <<"width">> => #{<<"integerValue">> => maps:get(<<"width">>, Response)}
                                      , <<"height">> => #{<<"integerValue">> => maps:get(<<"height">>, Response)}
                                      , <<"place">> => #{<<"stringValue">> => maps:get(<<"place">>, Response, null)}
                                      , <<"address">> => #{<<"stringValue">> => maps:get(<<"address">>, Response, null)}
                                      , <<"latitude">> => #{<<"doubleValue">> => maps:get(<<"latitude">>, Response, null)}
                                      , <<"longitude">> => #{<<"doubleValue">> => maps:get(<<"longitude">>, Response, null)}
                                      }
                   },
  JsonBodyReq = sr_json:encode(FirestoreBody),
  lager:info("Firestore Body Req ~p", [JsonBodyReq]),
  HTTPRequest1 = {FirestoreURL, Header1, Type1, binary_to_list(JsonBodyReq)},
  {ok, Response1} = httpc:request(Method1, HTTPRequest1, [], []),
  lager:info("Response Firestore: ~p", [Response1]),
  URL = "https://fcm.googleapis.com/fcm/send",
  Body = #{ <<"data">> =>FirestoreBody
          , <<"to">> => FirebaseToken
          },
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
  HTTPRequest = {URL, Header, Type, binary_to_list(Json)},
  _ = lager:info("Request Body ~p", [HTTPRequest]),
  {ok, Response2} = httpc:request(Method, HTTPRequest, HTTPOptions, Options),
  lager:info("Response ~p", [Response2]),
  % canillita_news_handler:notify(Entity),
  % _ = lager:info("Current state: ~p", [State]),
  {ok, {ServerKey, ServiceJson, AuthToken}};
handle_event(Event, State) ->
  _ = lager:info("Ignored event: ~p", [Event]),
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Arg, _State) ->
  ok.
