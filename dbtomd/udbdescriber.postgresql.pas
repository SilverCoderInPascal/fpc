unit udbdescriber.postgresql;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, sqldb, pqconnection, uDBDescriber;

type
  { PostgreSQL implementation }
  TPostgreSQLDescriber = class(TBaseDBDescriber)
  protected
    procedure CreateConnection; override;
    procedure SetConnectionParams(const Params: TStringList); override;
  public
    function GetTypeName: string; override;
    function GetTableListSQL: string; override;
    function GetColumnInfoSQL(const TableName: string): string; override;
    procedure WriteColumnRow(var F: TextFile; Query: TSQLQuery); override;
  end;


implementation

{ TPostgreSQLDescriber Implementation }

procedure TPostgreSQLDescriber.CreateConnection;
begin
  FConnection := TPQConnection.Create(nil);
end;

procedure TPostgreSQLDescriber.SetConnectionParams(const Params: TStringList);
begin
  with FConnection as TPQConnection do
  begin
    HostName := Params.Values['host'];
    DatabaseName := Params.Values['db'];
    UserName := Params.Values['user'];
    Password := Params.Values['pass'];
    FDatabaseName := Params.Values['db'];
  end;
end;

function TPostgreSQLDescriber.GetTypeName: string;
begin
  Result := 'PostgreSQL';
end;

function TPostgreSQLDescriber.GetTableListSQL: string;
begin
  Result := 'SELECT tablename FROM pg_tables WHERE schemaname = ''public''';
end;

function TPostgreSQLDescriber.GetColumnInfoSQL(const TableName: string): string;
begin
  Result :=
    'SELECT column_name, data_type, is_nullable, column_default ' +
    'FROM information_schema.columns ' +
    'WHERE table_name = ''' + TableName + ''' ' +
    'ORDER BY ordinal_position';
end;

procedure TPostgreSQLDescriber.WriteColumnRow(var F: TextFile; Query: TSQLQuery);
begin
  WriteLn(F, '| ', Query.FieldByName('column_name').AsString,
             ' | ', Query.FieldByName('data_type').AsString,
             ' | ', Query.FieldByName('is_nullable').AsString,
             ' | ', Query.FieldByName('column_default').AsString,
             ' | | ');
end;

end.

