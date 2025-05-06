unit BGPreprocess;

interface
type
	TCArgument = String;
	TCArgumentArray = Array of TCArgument;

	TCFunction = record
		ReturnType : String;
		FunctionName : String;
		Argument : TCArgumentArray;
	end;
	TCFunctionArray = Array of TCFunction;

	TCFile = record
		FileName : String;
		FunctionArray : TCFunctionArray;
	end;
	TCFileArray = Array of TCFile;

var
	CFiles : TCFileArray;

procedure BindgenPreprocess(Path : String);

implementation
uses
	RegExpr,
	Strutils,
	Sysutils,
	Classes;

procedure BindgenPreprocess(Path : String);
var
	S : String;
	F : Text;
	I : Integer;
	RE : TRegExpr;
	SRE : TRegExpr;
	FRE : TRegExpr;
	REStr : String;
	FunctionCount : Integer;
	Bracket : Integer;
	Count : Integer;
	Strings : TStringList;
const
	LEGAL : String = '[a-zA-Z_]';
begin
	REStr := '';

	FunctionCount := 0;
	Bracket := 0;

	SetLength(CFiles, Length(CFiles) + 1);
	CFiles[Length(CFiles) - 1].FileName := Path;
	SetLength(CFiles[Length(CFiles) - 1].FunctionArray, 0);

	(* Return type, Group 1 *)
	REStr := REStr + '(?:static[ \t]+)?(?:const[ \t]+)?((?:struct[ \t]+)?' + LEGAL + '+(?:[ \*]+)?)';
	REStr := REStr + '[ \t]+';

	(* Function name, Group 2 *)
	REStr := REStr + '(' + LEGAL + '+)';
	REStr := REStr + '[ \t]*';

	(* Arguments, Group 3 *)
	REStr := REStr + '\(([^\*].+)\)';

	(* End of Line *)
	REStr := REStr + '[ \t]*(;|\{)[ \t]*(.*)$';

	SRE := TRegExpr.Create('[ \t]*,[ \t]*');
	RE := TRegExpr.Create(REStr);
	AssignFile(F, Path);
	Reset(F);
	while not(EOF(F)) do
	begin
		ReadLn(F, S);
		if Bracket > 0 then
		begin
			Count := 0;
			for I := 1 to Length(S) do
			begin
				if S[I] = '{' then
				begin
					Count := Count + 1;
				end
				else if S[I] = '}' then
				begin
					Count := Count - 1;
				end
			end;
			Bracket := Bracket + Count;
			continue;
		end;
		if RE.Exec(S) and not(Trim(S)[1] = '#') then
		begin
			if RE.Match[4] = '{' then
			begin
				Count := 0;
				for I := 1 to Length(RE.Match[5]) do
				begin
					if (RE.Match[5][I] = '{') then
					begin
						Count := Count + 1;
					end
					else if (RE.Match[5][I] = '}') then
					begin
						Count := Count - 1;
					end
				end;
				Bracket := Bracket + 1 + Count;
				continue;
			end;
			FunctionCount := FunctionCount + 1;
			SetLength(CFiles[Length(CFiles) - 1].FunctionArray, FunctionCount);
			SetLength(CFiles[Length(CFiles) - 1].FunctionArray[FunctionCount - 1].Argument, 0);
			CFiles[Length(CFiles) - 1].FunctionArray[FunctionCount - 1].ReturnType := RE.Match[1];
			CFiles[Length(CFiles) - 1].FunctionArray[FunctionCount - 1].FunctionName := RE.Match[2];

			Strings := TStringList.Create();
			SRE.Split(RE.Match[3], Strings);
			for I := 0 to (Strings.Count - 1) do
			begin
				if Strings[I] = 'void' then
				begin
					continue;
				end;
				FRE := TRegExpr.Create('^(?:const[ \t]+)?' + LEGAL + '+[\* \t]*');
				FRE.Exec(Strings[I]);

				SetLength(CFiles[Length(CFiles) - 1].FunctionArray[FunctionCount - 1].Argument, Length(CFiles[Length(CFiles) - 1].FunctionArray[FunctionCount - 1].Argument) + 1);
				CFiles[Length(CFiles) - 1].FunctionArray[FunctionCount - 1].Argument[Length(CFiles[Length(CFiles) - 1].FunctionArray[FunctionCount - 1].Argument) - 1] := FRE.Match[0];

				FRE.Free();
			end;
			Strings.Free();
		end;
	end;
	CloseFile(F);
	RE.Free();
	SRE.Free();

	Write(IntToStr(FunctionCount) + ' functions');
end;

end.
