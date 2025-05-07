unit BGGenerate;

interface
uses
	BGTypes,
	DOM;

procedure BindgenGenerate();
procedure BindgenWriteHeader();
procedure BindgenWritePost();
procedure BindgenWriteMetatable(Path : String; Node : TDOMNode);
procedure BindgenGenerateFunction(Name : String; Entry : TCFunction);

implementation
uses
	BGPreprocess,
	BGConfig,
	BGVersion,
	BGRule,
	Dos,
	Sysutils;

var
	BGFile : TextFile;

procedure BindgenWriteHeader();
begin
	WriteLn(BGFile, '/**');
	WriteLn(BGFile, ' * Auto-generated using bindgen (https://github.com/pyrite-dev/bindgen)');
	WriteLn(BGFile, ' * Do not modify this directly! Might get overwritten.');
	WriteLn(BGFile, ' */');
	WriteLn(BGFile, '');
	WriteLn(BGFile, '#define LUA(wrap) (' + LuaState + ')');
	WriteLn(BGFile, '');
	WriteLn(BGFile, '/*** <Pre> ***/');
	WriteLn(BGFile, Pre);
	WriteLn(BGFile, '/*** </Pre> ***/');
end;

procedure BindgenWritePost();
var
	Year, Month, Day, WDay : Word;
	DateStr : String;
const
	DayStr : Array[0..6] of String[3] = (
		'Sun',
		'Mon',
		'Tue',
		'Wed',
		'Thu',
		'Fri',
		'Sat'
	);
	MonthStr : Array[1..12] of String[3] = (
		'Jan',
		'Feb',
		'Mar',
		'Apr',
		'May',
		'Jun',
		'Jul',
		'Aug',
		'Sep',
		'Oct',
		'Nov',
		'Dec'
	);
begin
	GetDate(Year, Month, Day, WDay);

	DateStr := DayStr[WDay] + ' ' + MonthStr[Month] + ' ' + IntToStr(Day) + ' ' + IntToStr(Year);

	WriteLn(BGFile, '/*** Generated at ' + DateStr + ' ***/');
end;

procedure BindgenGenerateFunction(Name : String; Entry : TCFunction);
var
	ArgStr : String;
	I : Integer;
begin
	ArgStr := '';

	for I := 0 to (Length(Entry.Argument) - 1) do
	begin
		if Length(ArgStr) > 0 then
		begin
			ArgStr := ArgStr + ', ';
		end;
		ArgStr := ArgStr + Entry.Argument[I];
	end;

	if Length(ArgStr) = 0 then
	begin
		ArgStr := '(void)';
	end
	else
	begin
		ArgStr := '(' + ArgStr + ')';
	end;

	WriteLn(BGFile, '/**');
	WriteLn(BGFile, ' * C: ' + Entry.ReturnType + ' ' + Entry.FunctionName + ArgStr);
	WriteLn(BGFile, ' */');
	WriteLn(BGFile, 'int bindgen_' + Name + '(lua_State* s){');
	WriteLn(BGFile, '	return 0;');
	WriteLn(BGFile, '}');
	WriteLn(BGFile, '');
end;

procedure BindgenWriteMetatable(Path : String; Node : TDOMNode);
var
	I : Integer;
	J : Integer;
	K : Integer;
	Functions : TCFunctionArray;
	Includes : Array of String;
	Found : Boolean;
	Name : String;
begin
	SetLength(Functions, 0);
	SetLength(Includes, 0);

	for I := 0 to (Length(CFiles) - 1) do
	begin
		for J := 0 to (Length(CFiles[I].FunctionArray) - 1) do
		begin
			if BindgenRule(@CFiles[I].FunctionArray[J], Node) then
			begin
				Found := False;
				for K := 0 to (Length(Includes) - 1) do
				begin
					if Includes[K] = CFiles[I].IncludePath then
					begin
						Found := True;
						break;
					end;
				end;

				if not(Found) then
				begin
					SetLength(Includes, Length(Includes) + 1);
					Includes[Length(Includes) - 1] := CFiles[I].IncludePath;
				end;

				SetLength(Functions, Length(Functions) + 1);
				Functions[Length(Functions) - 1] := CFiles[I].FunctionArray[J];
			end;
		end;
	end;

	AssignFile(BGFile, Path);
	Rewrite(BGFile);
	BindgenWriteHeader();

	WriteLn(BGFile, '');
	WriteLn(BGFile, '#include <lua.h>');
	WriteLn(BGFile, '#include <lauxlib.h>');

	WriteLn(BGFile, '');
	WriteLn(BGFile, '/*** Dependency ***/');
	for I := 0 to (Length(Includes) - 1) do
	begin
		WriteLn(BGFile, '#include <' + Includes[I] + '>');
	end;
	WriteLn(BGFile, '/******************/');

	WriteLn(BGFile, '');

	WriteLn(BGFile, '/**');
	WriteLn(BGFile, ' * Symbols:');
	for I := 0 to (Length(Functions) - 1) do
	begin
		WriteLn(BGFile, ' *   ' + Functions[I].FunctionName);
	end;
	WriteLn(BGFile, ' */');
	WriteLn(BGFile, '');

	for I := 0 to (Length(Functions) - 1) do
	begin
		BindgenGenerateFunction(TDOMElement(Node).GetAttribute('Lua') + '_' + Functions[I].FunctionName, Functions[I]);
	end;

	WriteLn(BGFile, 'void bindgen_' + TDOMElement(Node).GetAttribute('Lua') + '_init(' + LuaWrap + ' lua){');
	WriteLn(BGFile, '	luaL_newmetatable(LUA(lua), "' + TDOMElement(Node).GetAttribute('Lua') + '");');
	WriteLn(BGFile, '');
	for I := 0 to (Length(Functions) - 1) do
	begin
		Name := 'bindgen_' + TDOMElement(Node).GetAttribute('Lua') + '_' + Functions[I].FunctionName;
		WriteLn(BGFile, '	lua_pushstring(LUA(lua), "' + Functions[I].LuaName + '");');
		WriteLn(BGFile, '	lua_pushcfunction(LUA(lua), ' + Name + ');');
		WriteLn(BGFile, '	lua_settable(LUA(lua), -3);');
		WriteLn(BGFile, '');
	end;
	WriteLn(BGFile, '	lua_pushvalue(LUA(lua), -1);');
	WriteLn(BGFile, '	lua_setfield(LUA(lua), -2, "__index");');
	WriteLn(BGFile, '}');

	BindgenWritePost();
	CloseFile(BGFile);
end;

procedure BindgenGenerate();
var
	I : Integer;
	Child : TDOMNode;
begin
	for I := 0 to (Length(Metatable) - 1) do
	begin
		Child := Config.DocumentElement.FirstChild;
		while Assigned(Child) do
		begin
			if (Child.NodeName = 'Metatable') and (Metatable[I].Value = TDOMElement(Child).GetAttribute('Lua')) then
			begin
				BindgenWriteMetatable('bindgen_' + Metatable[I].Value + '.c', Child);
				break;
			end;
			Child := Child.NextSibling;
		end;
	end;
end;

end.
