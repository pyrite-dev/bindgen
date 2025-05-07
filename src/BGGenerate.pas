unit BGGenerate;

interface
procedure BindgenGenerate();

implementation
uses
	BGPreprocess,
	BGConfig,
	BGVersion,
	DOM;

procedure BindgenGenerate();
var
	Child : TDOMNode;
begin
	Child := Config.DocumentElement.FirstChild;
	while Assigned(Child) do
	begin
		if not(Child.NodeName = 'Pre') then
		begin
			
		end;
		Child := Child.NextSibling;
	end;
end;

end.
