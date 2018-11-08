-module(bert_sample).
-include_lib("bert/include/sample.hrl").
-include_lib("bert/include/bert.hrl").
-compile({parse_transform, bert_google}).
-compile({parse_transform, bert_validator}).

