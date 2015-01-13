-module(xparsexml).
-export([parse/1]).

parse(Bin) when is_binary(Bin) ->
    Bin1 = skip_declaration(Bin),
    {Tag, Rest} = tag(Bin1),
    <<>> = trim(Rest),
    Tag.

skip_declaration(<<"&lt;?xml", Bin/binary>>) ->
    [_,Rest] = binary:split(Bin, <<"?&gt;">>),
    Rest1 = trim(Rest),
    skip_declaration(Rest1);

skip_declaration(<<"&lt;!", Bin/binary>>) ->
    [_,Rest] = binary:split(Bin, <<"&gt;">>),
    trim(Rest);

skip_declaration(<<"&lt;",_/binary>> = Bin) -> 
    Bin;
skip_declaration(<<_,Bin/binary>>) -> 
    skip_declaration(Bin).

trim(<<" ",Bin/binary>>) -> trim(Bin);
trim(<<"\n",Bin/binary>>) -> trim(Bin);
trim(<<"\t",Bin/binary>>) -> trim(Bin);
trim(<<"\r",Bin/binary>>) -> trim(Bin);
trim(Bin) -> Bin.

tag(<<"&lt;", Bin/binary>>) ->
    [TagHeader1,Rest1] = binary:split(Bin, <<"&gt;">>),
    Len = size(TagHeader1)-1,

    case TagHeader1 of
	<<TagHeader:Len/binary, "/">> ->
	    {Tag, Attrs} = tag_header(TagHeader),
	    {{Tag,Attrs,[]}, Rest1};
	TagHeader ->
	    {Tag, Attrs} = tag_header(TagHeader),
	    {Content, Rest2} = tag_content(Rest1, Tag),
	    {{Tag,Attrs,Content}, Rest2}
    end.

tag_header(TagHeader) ->
    case binary:split(TagHeader, [<<" ">>]) of
	[Tag] -> {Tag, []};
	[Tag,Attrs] -> {Tag, tag_attrs(Attrs)}
    end.

tag_attrs(<<Blank,Attrs/binary>>)  when Blank == $  orelse Blank == $\n orelse Blank == $\t -> 
    tag_attrs(Attrs);
tag_attrs(<<>>) -> [];
tag_attrs(Attrs) ->
    case binary:split(Attrs,<<"=">>) of
	[Key,<<Quote:1/binary,Value1/binary>>] when Quote == <<"\"">> orelse Quote == <<"'">> ->
	    [Value,Rest] = binary:split(Value1,Quote),
	    [{Key,Value}|tag_attrs(Rest)]
    end.

tag_content(<<Blank,Bin/binary>>, Parent) when Blank == $  orelse Blank == $\n orelse Blank == $\r orelse Blank == $\t ->
    tag_content(Bin, Parent);

tag_content(<<"&lt;/", Bin1/binary>>, Parent) ->
    Len = size(Parent),
    <<Parent:Len/binary, "&gt;", Bin/binary>> = Bin1,
    {[], Bin};

tag_content(<<"&lt;",_/binary>> = Bin, Parent) ->
    {Tag, Rest1} = tag(Bin),
    {Content, Rest2} = tag_content(Rest1, Parent),
    {[Tag|Content], Rest2};

tag_content(Bin, Parent) ->
    [Text, Rest] = binary:split(Bin, <<"&lt;/",Parent/binary,"&gt;">>),
    {[Text],Rest}.
