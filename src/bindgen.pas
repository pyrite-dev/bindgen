program bindgen;

uses
	Sysutils,
	BGPreprocess;

procedure Recursive(Path : String);
var
	Info : TSearchRec;
	BGResult : Integer;
begin
	if DirectoryExists(Path) then
	begin
		if FindFirst(Path + '/*', faAnyFile, Info) = 0 then
		begin
			repeat
				if not ((Info.Name = '.') or (Info.Name = '..')) then
				begin
					Recursive(Path + '/' + Info.Name);
				end;
			until FindNext(Info) <> 0;
			FindClose(Info);
		end;
	end
	else if FileExists(Path) then
	begin
		Write(Path + '... ');
		BGResult := BindgenPreprocess(Path);
		if not(BGResult = 0) then
		begin
			WriteLn('Failed');
		end
		else
		begin
			Writeln('OK');
		end;
	end;
end;

var
	I : Integer;
begin
	WriteLn('GoldFish Lua binding generator');
	for I := 1 to ParamCount do
	begin
		Recursive(ParamStr(I));
	end;
end.
