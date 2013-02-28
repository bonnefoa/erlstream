%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Common records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(stream_configuration,
  { key, secret
    , token, tokenSecret
    , keywords, url
    , follow
  } ).

-record(status_request,
  { key, secret
    , token, tokenSecret
    , tweet_id
  } ).


-record(stream_state,
  {
    counter_pid
    , timer_pid
    , queue_mgr_pid
  } ).

-record(queue_mgr_state,
  {
    channel
    , publish
  } ).

