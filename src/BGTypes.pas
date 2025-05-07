unit BGTypes;

interface
type
	TCArgument = String;
	TCArgumentArray = Array of TCArgument;

	TCFunction = record
		ReturnType : String;
		FunctionName : String;
		LuaName : String;
		Argument : TCArgumentArray;
	end;
	TCFunctionArray = Array of TCFunction;
	PTCFunction = ^TCFunction;

	TCFile = record
		FileName : String;
		FunctionArray : TCFunctionArray;
		Root : String;
		IncludePath : String;
	end;
	TCFileArray = Array of TCFile;

	TBindgenEntry = record
		Key : String;
		Value : String;
	end;
	TBindgenMap = Array of TBindgenEntry;
	PTBindgenMap = ^TBindgenMap;

procedure BindgenMapSet(Map : PTBindgenMap; Key : String; Value : String);
function BindgenMapGet(Map : TBindgenMap; Key : String) : String;

implementation
procedure BindgenMapSet(Map : PTBindgenMap; Key : String; Value : String);
var
	I : Integer;
begin
	for I := 0 to (Length(Map^) - 1) do
	begin
		if (Map^)[I].Key = Key then
		begin
			(Map^)[I].Value := Value;
			Exit;
		end;
	end;
	SetLength(Map^, Length(Map^) + 1);
	(Map^)[Length(Map^) - 1].Key := Key;
	(Map^)[Length(Map^) - 1].Value := Value;
end;

function BindgenMapGet(Map : TBindgenMap; Key : String) : String;
var
	I : Integer;
begin
	BindgenMapGet := '';
	for I := 0 to (Length(Map) - 1) do
	begin
		if Map[I].Key = Key then
		begin
			BindgenMapGet := Map[I].Value;
			break;
		end;
	end;
end;

end.
