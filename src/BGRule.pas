unit BGRule;

interface
uses
	DOM,
	BGTypes;

function BindgenRule(Entry : PTCFunction; Node : TDOMNode) : Boolean;
function BindgenMatch(Entry : PTCFunction; Node : TDOMNode) : Boolean;

implementation
uses
	Sysutils,
	RegExpr;

function BindgenRule(Entry : PTCFunction; Node : TDOMNode) : Boolean;
var
	Child : TDOMNode;
	RE : TRegExpr;
	REStr : String;
begin
	BindgenRule := False;
	Child := Node.FirstChild;
	while Assigned(Child) do
	begin
		if Child.NodeName = 'Match' then
		begin
			BindgenRule := BindgenMatch(Entry, Child);
			if BindgenRule then
			begin
				REStr := String(TDOMElement(Child).GetAttribute('Name'));

				if REStr = '' then
				begin
					REStr := '^(.+)$';
				end;

				RE := TRegExpr.Create(REStr);
				RE.Exec(Entry^.FunctionName);
				Entry^.LuaName := RE.Match[1] + RE.Match[2] + RE.Match[3];
				RE.Free;
			end;
		end;
		if BindgenRule then
		begin
			break;
		end;
		Child := Child.NextSibling;
	end;
end;

function BindgenMatch(Entry : PTCFunction; Node : TDOMNode) : Boolean;
var
	Child : TDOMNode;
	ArgIndex : Integer;
	ArgType : String;
	I : Integer;
	RE : TRegExpr;
begin
	BindgenMatch := True;
	Child := Node.FirstChild;

	if Node.NodeName = 'Match' then
	begin
		for I := 0 to (Length(Entry^.Argument) - 1) do
		begin
			Entry^.ArgumentUse[I] := '';
		end;
	end;

	while Assigned(Child) do
	begin
		if Child.NodeName = 'Argument' then
		begin
			if TDOMElement(Child).GetAttribute('Index') = '' then
			begin
				ArgType := String(TDOMElement(Child).GetAttribute('Type'));
				BindgenMatch := False;
				for I := 0 to (Length(Entry^.Argument) - 1) do
				begin
					if Entry^.Argument[I] = ArgType then
					begin
						Entry^.ArgumentUse[I] := String(TDOMElement(Child).GetAttribute('Use'));
						BindgenMatch := True;
						break;
					end;
				end;
			end
			else
			begin
				ArgIndex := StrToInt(String(TDOMElement(Child).GetAttribute('Index')));
				ArgType := String(TDOMElement(Child).GetAttribute('Type'));
				if BindgenMatch and (ArgIndex >= Length(Entry^.Argument)) then
				begin
					BindgenMatch := False;
				end;
				if BindgenMatch and not(Entry^.Argument[ArgIndex] = ArgType) then
				begin
					BindgenMatch := False;
				end;
				if BindgenMatch then
				begin
					Entry^.ArgumentUse[ArgIndex] := String(TDOMElement(Child).GetAttribute('Use'));
				end;
			end;
		end
		else if Child.NodeName = 'Return' then
		begin
			ArgType := String(TDOMElement(Child).GetAttribute('Type'));
			BindgenMatch := Entry^.ReturnType = ArgType;
		end
		else if Child.NodeName = 'Name' then
		begin
			RE := TRegExpr.Create(String(TDOMElement(Child).GetAttribute('Pattern')));
			BindgenMatch := RE.Exec(Entry^.FunctionName);
			RE.Free();
		end
		else if Child.NodeName = 'Not' then
		begin
			BindgenMatch := not(BindgenMatch(Entry, Child));
		end;
		if not(BindgenMatch) then
		begin
			break;
		end;
		Child := Child.NextSibling;
	end;
end;

end.
