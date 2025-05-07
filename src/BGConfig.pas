unit BGConfig;

interface
uses
	DOM;

type
	TBindgenEntry = record
		Key : String;
		Value : String;
	end;
	TBindgenMap = Array of TBindgenEntry;

var
	Config : TXMLDocument;
	Pre : String;
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
			Pre := Child.NodeValue;
		end
		else
		begin
			BindgenConfigNode(Child);
		end;
		Child := Child.NextSibling;
	end;
end;

end.
