unit udbde.factory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function CreateDescriber(const DBTypeStr: string): IDBDescriber;

implementation

{ Factory function }
function CreateDescriber(const DBTypeStr: string): IDBDescriber;
begin
  if DBTypeStr = 'mysql' then
    Result := TMySQLDescriber.Create
  else if DBTypeStr = 'postgresql' then
    Result := TPostgreSQLDescriber.Create
  else if DBTypeStr = 'sqlite' then
    Result := TSQLiteDescriber.Create
  else
    Result := nil;
end;
end.

