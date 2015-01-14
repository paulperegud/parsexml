-module(xparsexml).
-export([parse/1]).

-include_lib("xmerl/include/xmerl.hrl").

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

b2a(Binary) ->
    list_to_atom(binary_to_list(Binary)).

rtag(Name, Attrs, Content) ->
    AName = b2a(Name),
    #xmlElement{name = AName, 
		expanded_name = AName, 
		attributes = Attrs, 
		content = Content}.

rattr(Key, Value) ->
    #xmlAttribute{name = b2a(Key), value = Value}.

rtext(Bin) ->
    #xmlText{value = Bin}.

tag(<<"&lt;", Bin/binary>>) ->
    [TagHeader1,Rest1] = binary:split(Bin, <<"&gt;">>),
    Len = size(TagHeader1)-1,

    case TagHeader1 of
	<<TagHeader:Len/binary, "/">> ->
	    {Tag, Attrs} = tag_header(TagHeader),
	    {rtag(Tag, Attrs, []), Rest1};
	TagHeader ->
	    {Tag, Attrs} = tag_header(TagHeader),
	    {Content, Rest2} = tag_content(Rest1, Tag),
	    {rtag(Tag, Attrs, Content), Rest2}
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
	    [rattr(Key, Value) | tag_attrs(Rest)]
    end.

%% skip blanks
tag_content(<<Blank,Bin/binary>>, Parent) when Blank == $  orelse Blank == $\n orelse Blank == $\r orelse Blank == $\t ->
    tag_content(Bin, Parent);

%% else: closing tag, no content
tag_content(<<"&lt;/", Bin1/binary>>, Parent) ->
    Len = size(Parent),
    <<Parent:Len/binary, "&gt;", Bin/binary>> = Bin1,
    {[], Bin};

%% else: we are opening some tag here
tag_content(<<"&lt;",_/binary>> = Bin, Parent) ->
    {Tag, Rest1} = tag(Bin), %% grab the tag
    {Content, Rest2} = tag_content(Rest1, Parent), %% after this tag there might be some content
    {[Tag|Content], Rest2};

%% else: no tag inside, assume text
tag_content(Bin, Parent) ->
    [Text, Rest] = binary:split(Bin, <<"&lt;/",Parent/binary,"&gt;">>),
    {[rtext(Text)],Rest}.
