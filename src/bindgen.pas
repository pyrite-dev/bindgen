program bindgen;

uses
	Sysutils,
	BGPreprocess,
	BGConfig;

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
	Config : String;
begin
	Config := 'bindgen.xml';

	WriteLn('GoldFish Lua binding generator');
	SetLength(CFiles, 0);

	I := 1;
	while I <= ParamCount do
	begin
		if (Length(ParamStr(I)) > 0) and (ParamStr(I)[1] = '-') then
		begin
			if (ParamStr(I) = '-C') or (ParamStr(I) = '--config') then
			begin
				I := I + 1;
				Config := ParamStr(I);
			end
			else
			begin
				WriteLn('Bad flag: ' + ParamStr(I));
				Halt(1);
			end;
		end
		else
		begin
			Recursive(ParamStr(I));
		end;
		I := I + 1;
	end;
	if FileExists(config) then
	begin
		WriteLn('Using config: ' + Config);
		BindgenConfig(Config);
	end;
end.
