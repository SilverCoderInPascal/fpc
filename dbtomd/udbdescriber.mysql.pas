unit uDBDescriber.MySql;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, sqldb, mysql80conn, uDBDescriber;
type
  { MySQL implementation }
  TMySQLDescriber = class(TBaseDBDescriber)
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
{ TMySQLDescriber Implementation }

procedure TMySQLDescriber.CreateConnection;
begin
  FConnection := TMySQL80Connection.Create(nil);
end;

procedure TMySQLDescriber.SetConnectionParams(const AParams: TStringList);
begin
  with FConnection as TMySQL80Connection do
  begin
    HostName := AParams.Values['host'];
    DatabaseName := AParams.Values['db'];
    UserName := AParams.Values['user'];
    Password := AParams.Values['pass'];
    FDatabaseName := AParams.Values['db'];
  end;
end;

function TMySQLDescriber.GetTypeName: string;
begin
  Result := 'MySQL';
end;

function TMySQLDescriber.GetTableListSQL: string;
begin
  Result := 'SHOW TABLES';
end;

function TMySQLDescriber.GetColumnInfoSQL(const TableName: string): string;
begin
  Result := 'SHOW COLUMNS FROM ' + TableName;
end;

procedure TMySQLDescriber.WriteColumnRow(var F: TextFile; Query: TSQLQuery);
begin
  WriteLn(F, '| ', Query.FieldByName('Field').AsString,
             ' | ', Query.FieldByName('Type').AsString,
             ' | ', Query.FieldByName('Null').AsString,
             ' | ', Query.FieldByName('Default').AsString,
             ' | ', Query.FieldByName('Key').AsString, ' |');
end;
end.

