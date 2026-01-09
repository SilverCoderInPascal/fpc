unit udbdescriber.sqlite;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, sqldb, uDBDescriber;

type

  {
  TInterfacedObject
  TBaseDBDescriber = class(TInterfacedObject, IDBDescriber)
  TSQLiteDescriber ... TPostgreSQLDescriber
  }

  { SQLite implementation }

  TSQLiteDescriber = class(TBaseDBDescriber)
  protected
    procedure CreateConnection; override;
    procedure SetConnectionParams(const AParams: TStringList); override;
  public
    function GetTypeName: string; override;
    function GetTableListSQL: string; override;
    function GetColumnInfoSQL(const TableName: string): string; override;
    procedure WriteColumnRow(var F: TextFile; Query: TSQLQuery); override;
  end;

implementation

uses strutils, sqlite3conn;

{ TSQLiteDescriber Implementation }

procedure TSQLiteDescriber.CreateConnection;
begin
  FConnection := TSQLite3Connection.Create(nil);
end;

procedure TSQLiteDescriber.SetConnectionParams(const AParams: TStringList);
begin
  WriteLn('Params=', AParams.Text);
  with FConnection as TSQLite3Connection do
  begin
    DatabaseName := AParams.Values['db'];
    FDatabaseName := AParams.Values['db'];
    WriteLn('DatabaseName=', DatabaseName);
  end;
end;

function TSQLiteDescriber.GetTypeName: string;
begin
  Result := 'SQLite';
end;

function TSQLiteDescriber.GetTableListSQL: string;
begin
  Result := 'SELECT name FROM sqlite_master WHERE type = ''table''';
end;

function TSQLiteDescriber.GetColumnInfoSQL(const TableName: string): string;
begin
  Result := 'PRAGMA table_info(' + TableName + ')';
end;

procedure TSQLiteDescriber.WriteColumnRow(var F: TextFile; Query: TSQLQuery);
begin
  WriteLn(F, '| ', Query.FieldByName('name').AsString,
             ' | ', Query.FieldByName('type').AsString,
             ' | ', IfThen(Query.FieldByName('notnull').AsInteger = 0, 'YES', 'NO'),
             ' | ', Query.FieldByName('dflt_value').AsString,
             ' | ', IfThen(Query.FieldByName('pk').AsInteger = 1, 'PRI', ''), ' |');
end;

end.

