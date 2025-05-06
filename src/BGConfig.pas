unit BGConfig;

interface
uses
	DOM;

var
	Config : TXMLDocument;

procedure BindgenConfig(Path : String);

implementation
uses
	XMLRead,
	BGPreprocess;

procedure BindgenConfigNode(Node : TDOMNode);
var
	Child : TDOMNode;
begin
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
		BindgenConfigNode(Child);
		Child := Child.NextSibling;
	end;
end;

end.
