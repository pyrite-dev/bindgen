unit BGInit;

interface
procedure BindgenInit();

implementation
uses
	BGPreprocess,
	BGConfig;

procedure BindgenInit();
begin
	SetLength(CFiles, 0);
end;

end.
