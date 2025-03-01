program teststringlist;

uses
  Classes;

const
  MyFile = './testlist.txt';

procedure DebugList(AList: TStrings);
var
  I:Integer;
  S:String;
begin
  WriteLn('Contents of List =>');
  Write('  ');

  for I := 0 to AList.Count-1 do
  begin
    if I > 0 then
      Write(', ');
    Write(AList.Strings[I]);
  end;
  WriteLn;
end;

procedure FillList(AList: TStrings);
begin
  AList.Add('Apple');
  AList.Add('Pear');
  AList.Add('Banana');
  AList.Add('Plum');
  AList.Add('Mango');
  AList.Add('Grapes');
end;

procedure StringListLoad;
var
  LItems: TStringList;
begin
  WriteLn('Loading Items from StringList');
  LItems := TStringList.Create;
  LItems.LoadFromFile(MyFile);
  DebugList(LItems);
  LItems.Free;
  WriteLn;
end;

procedure StringListSave;
var
  LItems: TStringList;
  S:String;
begin
  WriteLn('Saving Items to StringList');
  LItems := TStringList.Create;
  FillList(LItems);
  DebugList(LItems);
  LItems.SaveToFile(MyFile);
  LItems.Free;
  WriteLn;
end;

procedure StringListSorted;
var
  LItems: TStringList;
begin
  WriteLn('Items in Sorted StringList');
  LItems := TStringList.Create;
  LItems.Sorted := True;
  FillList(LItems);
  DebugList(LItems);
  LItems.Free;
  WriteLn;
end;

procedure StringListLookup;
var
  LItems: TStringList;
  LPos:Integer;
begin
  WriteLn('Searching for Items in StringList');
  LItems := TStringList.Create;
  LItems.Sorted := True;
  FillList(LItems);
  DebugList(LItems);
  (*
  LPos := LItems.IndexOf('Plum');
  if LPos >= 0 then
    WriteLn('Plum found at ', LPos);
  LPos := LItems.IndexOf('Peach');
  WriteLn('Peach found at ', LPos);
  *)

  if LItems.Find('Plum', LPos) then
    WriteLn('Plum found at ', LPos);

  LItems.Free;
  WriteLn;
end;

procedure StringListWithKeyValue;
var
  LItems: TStringList;
  LPos:Integer;
  LValue:String;
begin
  WriteLn('StringList with Key/Value Pairs');
  LItems := TStringList.Create;
  LItems.Values['username'] := 'tim';
  LItems.Values['role'] := 'programmer';
  LItems.Values['team'] := '12B';
  DebugList(LItems);

  LPos := LItems.IndexOfName('role');
  WriteLn('role found at ', LPos);

  LValue := LItems.Values['role'];
  WriteLn('role value is ', LValue);

  LItems.Clear;
  DebugList(LItems);

  LItems.Free;
  WriteLn;
end;

begin
  StringListSave;
  StringListLoad;
  StringListSorted;
  StringListLookup;
  StringListWithKeyValue;
end.

