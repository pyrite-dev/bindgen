unit BGGenerate;

interface
uses
	BGTypes,
	DOM;

function BindgenGetType(Name : String) : String;
procedure BindgenGenerate();
procedure BindgenWriteHeader();
procedure BindgenWritePost();
procedure BindgenWriteMetatable(Path : String; Node : TDOMNode);
procedure BindgenWriteTemplate(Path : String; Node : TDOMNode);
procedure BindgenWriteGeneric(Path : String; Node : TDOMNode);
procedure BindgenGenerateFunction(Name : String; Entry : TCFunction);
procedure BindgenReturnArray(TypeName : String);
procedure BindgenGetArray(TypeName : String; VarName : String; ArgIndex : Integer);

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
	Functions : TCFunctionArray;
	Includes : Array of String;

const
	TypeTable : Array of String = (
		'double', 'number', 'Number',
		'int', 'integer', 'Integer',
		'const char*', 'string', 'String',
		'char*', 'string', 'String'
	);

procedure BindgenReturnArray(TypeName : String);
var
	I : Integer;
	J : Integer;
	Child : TDOMNode;
begin
	Child := Config.DocumentElement.FirstChild;
	while Assigned(Child) do
	begin
		if (Child.NodeName = 'Array') and (String(TDOMElement(Child).GetAttribute('Name')) = TypeName) then
		begin
			Child := Child.FirstChild;
			WriteLn(BGFile, '	lua_newtable(s);');
			J := 1;
			while Assigned(Child) do
			begin
				I := 0;
				while I < Length(TypeTable) do
				begin
					if TypeTable[I + 2] = String(Child.NodeName) then
					begin
						WriteLn(BGFile, '	lua_push' + TypeTable[I + 1] + '(s, ret.' + String(TDOMElement(Child).GetAttribute('Field')) + ');');
						WriteLn(BGFile, '	lua_rawseti(s, -2, ' + IntToStr(J) + ');');
						break;
					end;
					I := I + 3;
				end;
				J := J + 1;
				Child := Child.NextSibling;
			end;
			break;
		end;
		Child := Child.NextSibling;
	end;
end;

procedure BindgenGetArray(TypeName : String; VarName : String; ArgIndex : Integer);
var
	I : Integer;
	J : Integer;
	Child : TDOMNode;
	Default : String;
	Indent : String;
begin
	Child := Config.DocumentElement.FirstChild;
	while Assigned(Child) do
	begin
		if (Child.NodeName = 'Array') and (String(TDOMElement(Child).GetAttribute('Name')) = TypeName) then
		begin
			Default := String(TDOMElement(Child).GetAttribute('Default'));
			Indent := '';
			if not(Default = '') then
			begin
				Indent := '	';
				WriteLn(BGFile, '	if(lua_gettop(s) >= ' + IntToStr(ArgIndex) + '){');
			end;

			Child := Child.FirstChild;
			J := 1;
			while Assigned(Child) do
			begin
				I := 0;
				while I < Length(TypeTable) do
				begin
					if TypeTable[I + 2] = String(Child.NodeName) then
					begin
						WriteLn(BGFile, Indent + '	lua_rawgeti(s, ' + IntToStr(ArgIndex) + ', ' + IntToStr(J) + ');');
						WriteLn(BGFile, Indent + '	' + VarName + '.' + String(TDOMElement(Child).GetAttribute('Field')) + ' = luaL_check' + TypeTable[I + 1] + '(s, -1);');
						WriteLn(BGFile, Indent + '	lua_pop(s, 1);');
						break;
					end;
					I := I + 3;
				end;
				J := J + 1;
				Child := Child.NextSibling;
			end;

			if not(Default = '') then
			begin
				Indent := '	';
				WriteLn(BGFile, '	} else {');
				WriteLn(BGFile, '		' + VarName + ' = ' + Default + ';');
				WriteLn(BGFile, '	}');
			end;
			break;
		end;
		Child := Child.NextSibling;
	end;
end;

function BindgenGetType(Name : String) : String;
var
	I : Integer;
begin
	BindgenGetType := '';
	I := 0;
	while I < Length(TypeTable) do
	begin
		if TypeTable[I] = Name then
		begin
			BindgenGetType := TypeTable[I + 1];
			break;
		end;
		I := I + 3;
	end;
end;

procedure BindgenWriteHeader();
begin
	WriteLn(BGFile, '/**');
	WriteLn(BGFile, ' * Auto-generated using bindgen (https://github.com/pyrite-dev/bindgen)');
	WriteLn(BGFile, ' * Do not modify this directly! Might get overwritten.');
	WriteLn(BGFile, ' */');
	WriteLn(BGFile, '');
	WriteLn(BGFile, '#include <bindgen.h>');
	WriteLn(BGFile, '#include <lua.h>');
	WriteLn(BGFile, '#include <lauxlib.h>');
	WriteLn(BGFile, '');
	WriteLn(BGFile, '#ifndef LUA');
	WriteLn(BGFile, '#define LUA(wrap) (' + LuaState + ')');
	WriteLn(BGFile, '#endif');
	WriteLn(BGFile, '');
	WriteLn(BGFile, '/*** <Pre> ***/');
	WriteLn(BGFile, Pre);
	WriteLn(BGFile, '/*** </Pre> ***/');
	WriteLn(BGFile, '');
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

	WriteLn(BGFile, '');
	WriteLn(BGFile, '/*** Generated at ' + DateStr + ' ***/');
end;

procedure BindgenGenerateFunction(Name : String; Entry : TCFunction);
var
	ArgStr : String;
	I : Integer;
	PN : Integer;
	LuaName : String;
	LuaData : String;
	IsArray : Boolean;
	Child : TDOMNode;
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
	PN := 0;
	for I := 0 to (Length(Entry.Argument) - 1) do
	begin
		LuaName := '';
		IsArray := False;
		Child := Config.DocumentElement.FirstChild;
		while Assigned(Child) do
		begin
			if (Child.NodeName = 'Array') and (String(TDOMElement(Child).GetAttribute('Name')) = Entry.Argument[I]) then
			begin
				IsArray := True;
				break;
			end;
			Child := Child.NextSibling;
		end;
		
		Child := Config.DocumentElement.FirstChild;
		while Assigned(Child) do
		begin
			if (Child.NodeName = 'Metatable') and (String(TDOMElement(Child).GetAttribute('C')) = Entry.Argument[I]) then
			begin
				LuaName := String(TDOMElement(Child).GetAttribute('Lua'));
				break;
			end;
			Child := Child.NextSibling;
		end;

		if not(IsArray) and (Entry.ArgumentUse[I] = '') then
		begin
			if LuaName = '' then
			begin
				LuaData := BindgenGetType(Entry.Argument[I]);
				WriteLn(BGFile, '	' + Entry.Argument[I] + ' param' + IntToStr(I) + ' = (' + Entry.Argument[I] + ')luaL_check' + LuaData + '(s, ' + IntToStr(PN + 1) + ');');
			end
			else
			begin
				WriteLn(BGFile, '	' + Entry.Argument[I] + '* param' + IntToStr(I) + ' = luaL_checkudata(s, ' + IntToStr(PN + 1) + ', "' + LuaName + '");');
			end;
			PN := PN + 1;
		end
		else if IsArray then
		begin
			WriteLn(BGFile, '	' + Entry.Argument[I] + ' param' + IntToStr(I) + ';');
		end
		else
		begin
			if LuaName = '' then
			begin
				WriteLn(BGFile, '	' + Entry.Argument[I] + ' param' + IntToStr(I) + ';');
			end
			else
			begin
				WriteLn(BGFile, '	' + Entry.Argument[I] + '* param' + IntToStr(I) + ';');
			end;
		end;
	end;
	WriteLn(BGFile, '	' + LuaWrap + ' wrap;');
	if not(Entry.ReturnType = 'void') then
	begin
		WriteLn(BGFile, '	' + Entry.ReturnType + ' ret;');
		LuaName := '';
		Child := Config.DocumentElement.FirstChild;
		while Assigned(Child) do
		begin
			if (Child.NodeName = 'Metatable') and (String(TDOMElement(Child).GetAttribute('C')) = Entry.ReturnType) then
			begin
				LuaName := String(TDOMElement(Child).GetAttribute('Lua'));
				break;
			end;
			Child := Child.NextSibling;
		end;

		if not(LuaName = '') then
		begin
			WriteLn(BGFile, '	' + Entry.ReturnType + '* lret;');
		end;
	end;
	WriteLn(BGFile, '');

	WriteLn(BGFile, '	lua_getglobal(s, "_LUA_WRAP");');
	WriteLn(BGFile, '	wrap = lua_touserdata(s, -1);');
	WriteLn(BGFile, '	lua_pop(s, 1);');
	WriteLn(BGFile, '');

	PN := 0;
	for I := 0 to (Length(Entry.Argument) - 1) do
	begin
		LuaName := '';
		IsArray := False;
		Child := Config.DocumentElement.FirstChild;
		while Assigned(Child) do
		begin
			if (Child.NodeName = 'Array') and (String(TDOMElement(Child).GetAttribute('Name')) = Entry.Argument[I]) then
			begin
				IsArray := True;
				break;
			end;
			Child := Child.NextSibling;
		end;

		Child := Config.DocumentElement.FirstChild;
		while Assigned(Child) do
		begin
			if (Child.NodeName = 'Metatable') and (String(TDOMElement(Child).GetAttribute('C')) = Entry.Argument[I]) then
			begin
				LuaName := String(TDOMElement(Child).GetAttribute('Lua'));
				break;
			end;
			Child := Child.NextSibling;
		end;

		if not(IsArray) and not(Entry.ArgumentUse[I] = '') then
		begin
			if LuaName = '' then
			begin
				WriteLn(BGFile, '	param' + IntToStr(I) + ' = ' + Entry.ArgumentUse[i] + ';');
			end
			else
			begin
				WriteLn(BGFile, '	param' + IntToStr(I) + ' = &' + Entry.ArgumentUse[i] + ';');
			end;
		end
		else if IsArray then
		begin
			BindgenGetArray(Entry.Argument[I], 'param' + IntToStr(I), PN + 1);
			PN := PN + 1;
		end
		else
		begin
			PN := PN + 1;
		end;
	end;
	WriteLn(BGFile, '');
	Write(BGFile, '	');
	if not(Entry.ReturnType = 'void') then
	begin
		Write(BGFile, 'ret = (' + Entry.ReturnType + ')');
	end;
	Write(BGFile, Entry.FunctionName + '(');
	for I := 0 to (Length(Entry.Argument) - 1) do
	begin
		LuaName := '';
		Child := Config.DocumentElement.FirstChild;
		while Assigned(Child) do
		begin
			if (Child.NodeName = 'Metatable') and (String(TDOMElement(Child).GetAttribute('C')) = Entry.Argument[I]) then
			begin
				LuaName := String(TDOMElement(Child).GetAttribute('Lua'));
				break;
			end;
			Child := Child.NextSibling;
		end;

		if I > 0 then
		begin
			Write(BGFile, ', ');
		end;

		if LuaName = '' then
		begin
			Write(BGFile, 'param' + IntToStr(I));
		end
		else
		begin
			Write(BGFile, '*param' + IntToStr(I));
		end;
	end;
	WriteLn(BGFile, ');');

	if not(Entry.ReturnType = 'void') then
	begin
		IsArray := False;
		Child := Config.DocumentElement.FirstChild;
		while Assigned(Child) do
		begin
			if (Child.NodeName = 'Array') and (String(TDOMElement(Child).GetAttribute('Name')) = Entry.ReturnType) then
			begin
				IsArray := True;
				BindgenReturnArray(Entry.ReturnType);
				break;
			end;
			Child := Child.NextSibling;
		end;

		LuaName := '';
		Child := Config.DocumentElement.FirstChild;
		while Assigned(Child) do
		begin
			if (Child.NodeName = 'Metatable') and (String(TDOMElement(Child).GetAttribute('C')) = Entry.ReturnType) then
			begin
				if not(String(TDOMElement(Child).GetAttribute('Valid')) = '') then
				begin
					WriteLn(BGFile, '	if(!(' + String(TDOMElement(Child).GetAttribute('Valid')) + ')) return 0;');
				end;
				LuaName := String(TDOMElement(Child).GetAttribute('Lua'));
				break;
			end;
			Child := Child.NextSibling;
		end;

		if not(IsArray) then
		begin
			if LuaName = '' then
			begin
				LuaData := BindgenGetType(Entry.ReturnType);
				WriteLn(BGFile, '	lua_push' + LuaData + '(s, ret);');
			end
			else
			begin
				WriteLn(BGFile, '	lret = lua_newuserdata(s, sizeof(*lret));');
				WriteLn(BGFile, '	luaL_getmetatable(s, "' + LuaName + '");');
				WriteLn(BGFile, '	lua_setmetatable(s, -2);');
				WriteLn(BGFile, '	*lret = ret;');
			end;
		end;

		WriteLn(BGFile, '');
		WriteLn(BGFile, '	return 1;');
	end
	else
	begin
		WriteLn(BGFile, '');
		WriteLn(BGFile, '	return 0;');
	end;
	WriteLn(BGFile, '}');
	WriteLn(BGFile, '');
end;

procedure BindgenWriteMetatable(Path : String; Node : TDOMNode);
var
	I : Integer;
	Name : String;
begin
	BindgenWriteTemplate(Path, Node);
	WriteLn(BGFile, 'void bindgen_' + String(TDOMElement(Node).GetAttribute('Lua')) + '_init(' + LuaWrap + ' lua){');
	WriteLn(BGFile, '	luaL_newmetatable(LUA(lua), "' + String(TDOMElement(Node).GetAttribute('Lua')) + '");');
	WriteLn(BGFile, '');
	for I := 0 to (Length(Functions) - 1) do
	begin
		Name := 'bindgen_' + String(TDOMElement(Node).GetAttribute('Lua')) + '_' + Functions[I].FunctionName;
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

procedure BindgenWriteGeneric(Path : String; Node : TDOMNode);
var
	I : Integer;
	Name : String;
begin
	BindgenWriteTemplate(Path, Node);
	WriteLn(BGFile, 'void bindgen_' + String(TDOMElement(Node).GetAttribute('Name')) + '_init(' + LuaWrap + ' lua){');
	WriteLn(BGFile, '	lua_pushstring(LUA(lua), "' + String(TDOMElement(Node).GetAttribute('Name')) + '");');
	WriteLn(BGFile, '	lua_newtable(LUA(lua));');
	WriteLn(BGFile, '');
	for I := 0 to (Length(Functions) - 1) do
	begin
		Name := 'bindgen_' + String(TDOMElement(Node).GetAttribute('Name')) + '_' + Functions[I].FunctionName;
		WriteLn(BGFile, '	lua_pushstring(LUA(lua), "' + Functions[I].LuaName + '");');
		WriteLn(BGFile, '	lua_pushcfunction(LUA(lua), ' + Name + ');');
		WriteLn(BGFile, '	lua_settable(LUA(lua), -3);');
		WriteLn(BGFile, '');
	end;
	WriteLn(BGFile, '	lua_settable(LUA(lua), -3);');
	WriteLn(BGFile, '}');

	BindgenWritePost();
	CloseFile(BGFile);
end;

procedure BindgenWriteTemplate(Path : String; Node : TDOMNode);
var
	I : Integer;
	J : Integer;
	K : Integer;
	Found : Boolean;
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
		BindgenGenerateFunction((String(TDOMElement(Node).GetAttribute('Lua')) + String(TDOMElement(Node).GetAttribute('Name'))) + '_' + Functions[I].FunctionName, Functions[I]);
		WriteLn(String(TDOMElement(Node).GetAttribute('Lua')) + String(TDOMElement(Node).GetAttribute('Name')) + '.' + Functions[I].LuaName);
	end;
end;

procedure BindgenGenerate();
var
	Child : TDOMNode;
begin
	Child := Config.DocumentElement.FirstChild;
	while Assigned(Child) do
	begin
		if Child.NodeName = 'Metatable' then
		begin
			BindgenWriteMetatable(OutputRoot + '/bindgen_' + String(TDOMElement(Child).GetAttribute('Lua')) + '.c', Child);
		end;
		Child := Child.NextSibling;
	end;

	Child := Config.DocumentElement.FirstChild;
	while Assigned(Child) do
	begin
		if Child.NodeName = 'Group' then
		begin
			BindgenWriteGeneric(OutputRoot + '/bindgen_' + String(TDOMElement(Child).GetAttribute('Name')) + '.c', Child);
		end;
		Child := Child.NextSibling;
	end;

	AssignFile(BGFile, OutputRoot + '/bindgen.c');
	Rewrite(BGFile);
	BindgenWriteHeader();

	WriteLn(BGFile, 'void bindgen_init(' + LuaWrap + ' lua){');
	WriteLn(BGFile, '	lua_pushlightuserdata(LUA(lua), lua);');
	WriteLn(BGFile, '	lua_setglobal(LUA(lua), "_LUA_WRAP");');

	Child := Config.DocumentElement.FirstChild;
	while Assigned(Child) do
	begin
		if Child.NodeName = 'Metatable' then
		begin
			WriteLn(BGFile, '	bindgen_' + String(TDOMElement(Child).GetAttribute('Lua')) + '_init(lua);');
		end;
		Child := Child.NextSibling;
	end;

	WriteLn(BGFile, '');
	WriteLn(BGFile, '	lua_newtable(LUA(lua));');

	Child := Config.DocumentElement.FirstChild;
	while Assigned(Child) do
	begin
		if Child.NodeName = 'Group' then
		begin
			WriteLn(BGFile, '	bindgen_' + String(TDOMElement(Child).GetAttribute('Name')) + '_init(lua);');
		end;
		Child := Child.NextSibling;
	end;

	WriteLn(BGFile, '	lua_setglobal(LUA(lua), "' + LuaRoot + '");');
	WriteLn(BGFile, '}');

	BindgenWritePost();
	CloseFile(BGFile);

	AssignFile(BGFile, OutputRoot + '/bindgen.h');
	Rewrite(BGFile);
	WriteLn(BGFile, '#ifndef __BINDGEN_H__');
	WriteLn(BGFile, '#define __BINDGEN_H__');
	WriteLn(BGFile, '#ifdef __cplusplus');
	WriteLn(BGFile, 'extern "C" {');
	WriteLn(BGFile, '#endif');
	BindgenWriteHeader();

	Child := Config.DocumentElement.FirstChild;
	while Assigned(Child) do
	begin
		if Child.NodeName = 'Metatable' then
		begin
			WriteLn(BGFile, 'void bindgen_' + String(TDOMElement(Child).GetAttribute('Lua')) + '_init(' + LuaWrap + ' lua);');
		end
		else if Child.NodeName = 'Group' then
		begin
			WriteLn(BGFile, 'void bindgen_' + String(TDOMElement(Child).GetAttribute('Name')) + '_init(' + LuaWrap + ' lua);');
		end;
		Child := Child.NextSibling;
	end;

	WriteLn(BGFile, 'void bindgen_init(' + LuaWrap + ' lua);');

	WriteLn(BGFile, '');
	WriteLn(BGFile, '#ifdef __cplusplus');
	WriteLn(BGFile, '}');
	WriteLn(BGFile, '#endif');
	BindgenWritePost();
	WriteLn(BGFile, '#endif');
	CloseFile(BGFile);
end;

end.
