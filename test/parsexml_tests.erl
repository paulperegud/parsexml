-module(parsexml_tests).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


parse_test_() -> 
    A = #xmlElement{name = html, expanded_name = html},
    B = A#xmlElement{attributes=[#xmlAttribute{name = xmlns, value = <<"w3c">>}]},
    C = A#xmlElement{attributes=[#xmlAttribute{name = k, value = <<"v">>}]},
  [
  ?_assertEqual(A,
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>))
  ,?_assertEqual(A,
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n <html></html>\n">>))
  ,?_assertEqual(A,
    parsexml:parse(<<"\n\n\n<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html></html>\n">>))
  ,?_assertEqual(A,
    parsexml:parse(<<"\n<html></html>\n">>))
  ,?_assertEqual(A,
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html ></html>\n">>))
  ,?_assertEqual(B, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html xmlns=\"w3c\"></html>\n">>))
  ,?_assertEqual(B, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html xmlns=\"w3c\" ></html>\n">>))
  ,?_assertEqual(B, 
    parsexml:parse(<<"<html xmlns='w3c' />\n">>))
  ,?_assertEqual(A, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html/>\n">>))
  ,?_assertEqual(A, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html />\n">>))
  ,?_assertEqual(C, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k=\"v\"/>\n">>))
  ,?_assertEqual(C, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k=\"v\" />\n">>))
  ,?_assertEqual(C, 
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<html k=\"v\" />\r\n">>))
  ,?_assertEqual(C,
    parsexml:parse(<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE some_dtd SYSTEM \"example.dtd\">\n<html k=\"v\" />\n">>))
  ].

simple_test() ->
    checkparse("xtest2.xml").

checkparse(PrivPath) ->
    Path = "../test/" ++ PrivPath,
    {ok, Bin} = file:read_file(Path),
    parsexml:parse(Bin).

%% parse2_test() ->
%%   {ok, Bin} = file:read_file("../test/test1.xml"),
%%   ?assertEqual(
%%     {<<"p:program">>, [{<<"channel">>,<<"otv">>}, {<<"xmlns:p">>,<<"http://otv/ns">>}],  [
%%       {<<"p:day">>, [{<<"date">>,<<"2011-10-19">>}], [
%%         {<<"p:item">>, [{<<"id">>,<<"2877">>},{<<"href">>,<<"http://otv/15/">>},{<<"title">>,<<"New">>}],[]},
%%         {<<"p:item">>, [{<<"id">>,<<"2878">>},{<<"href">>,<<"http://otv/16/">>},
%%           {<<"title">>,<<"Chan &quot;Morning&quot; (0+)">>}],[
%%           <<"Morning &lt;chan&gt;,…">>
%%         ]}
%%       ]}
%%     ]},
%%   parsexml:parse(Bin)
%%   ).
