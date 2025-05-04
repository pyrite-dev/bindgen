program bindgen;

uses
	Sysutils,
	BGPreprocess;

procedure Recursive(Path : String);
var
	Info : TSearchRec;
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
		BindgenPreprocess(Path);
		WriteLn('');
	end;
end;

var
	I : Integer;
begin
	WriteLn('GoldFish Lua binding generator');
	SetLength(CFiles, 0);
	for I := 1 to ParamCount do
	begin
		Recursive(ParamStr(I));
	end;
end.
