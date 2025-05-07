program bindgen;

uses
	Sysutils,
	BGPreprocess,
	BGConfig,
	BGVersion,
	BGGenerate,
	BGInit;

procedure Recursive(Root : String; Path : String);
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
					Recursive(Root, Path + '/' + Info.Name);
				end;
			until FindNext(Info) <> 0;
			FindClose(Info);
		end;
	end
	else if FileExists(Path) then
	begin
		BindgenPreprocess(Root, Path);
	end;
end;

var
	I : Integer;
	Config : String;
begin
	Config := 'bindgen.xml';

	WriteLn('GoldFish Lua binding generator ' + BindgenVersion);
	BindgenInit();

	WriteLn('** Phase 1 - Preprocess files');
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
			Recursive(ParamStr(I), ParamStr(I));
		end;
		I := I + 1;
	end;

	WriteLn('** Phase 2 - Read config');
	if FileExists(config) then
	begin
		BindgenConfig(Config);
	end
	else
	begin
		WriteLn('Config not found');
		Halt(1);
	end;

	WriteLn('** Phase 3 - Generate C source/header');
	BindgenGenerate();
end.
