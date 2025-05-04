unit BGPreprocess;

interface
function BindgenPreprocess(Path : String) : Integer;

implementation
uses
	RegExpr,
	Strutils;

function BindgenPreprocess(Path : String) : Integer;
var
	S : String;
	F : Text;
	RE : TRegExpr;
	REStr : String;
const
	LEGAL : String = '[a-zA-Z_]';
begin
	REStr := '';

	(* Return type, Group 1 *)
	REStr := REStr + '((?:struct[ \t]+)?' + LEGAL + '+(?:[ \*]+)?)';
	REStr := REStr + '[ \t]+';

	(* Function name, Group 2 *)
	REStr := REStr + '(' + LEGAL + '+)';
	REStr := REStr + '[ \t]*';

	(* Arguments, Group 3 *)
	REStr := REStr + '\(([^\*].+)\)';

	(* End of Line *)
	REStr := REStr + '[ \t]*;[ \t]*$';

	RE := TRegExpr.Create(REStr);
	AssignFile(F, Path);
	Reset(F);
	while not(EOF(F)) do
	begin
		ReadLn(F, S);
		if RE.Exec(S) then
		begin
		end;
	end;
	CloseFile(F);
	RE.Free();
end;

end.
