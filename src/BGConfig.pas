unit BGConfig;

interface
uses
	DOM,
	BGTypes;

var
	Config : TXMLDocument;
	Pre : String;
	LuaRoot : String;
	LuaWrap : String;
	LuaState : String;
	Metatable : TBindgenMap;

procedure BindgenConfig(Path : String);

implementation
uses
	XMLRead;

procedure BindgenConfigNode(Node : TDOMNode);
var
	Child : TDOMNode;
begin
	if Node.NodeName = 'Metatable' then
	begin 
		BindgenMapSet(@Metatable, TDOMElement(Node).GetAttribute('C'), TDOMElement(Node).GetAttribute('Lua'));
	end;
	Child := Node.FirstChild;
	while Assigned(Child) do
	begin
		Child := Child.NextSibling;
	end;
end;

procedure BindgenConfig(Path : String);
var
	Child : TDOMNode;
begin
	ReadXMLFile(Config, Path);
	Child := Config.DocumentElement.FirstChild;
	while Assigned(Child) do
	begin
		if Child.NodeName = 'Pre' then
		begin
			Pre := Child.TextContent;
		end
		else if Child.NodeName = 'LuaRoot' then
		begin
			LuaRoot := Child.TextContent;
		end
		else if Child.NodeName = 'LuaWrap' then
		begin
			LuaWrap := Child.TextContent;
		end
		else if Child.NodeName = 'LuaState' then
		begin
			LuaState := Child.TextContent;
		end
		else
		begin
			BindgenConfigNode(Child);
		end;
		Child := Child.NextSibling;
	end;
end;

end.
