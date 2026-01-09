program dbtomd;

{$mode objfpc}{$H+}
{$interfaces corba}

uses
  SysUtils, Classes,
  sqldb,
  strutils,
  uDBDescriber,
  uDBDescriber.Factory;



{ Main Application Logic }

procedure ShowUsage;
begin
  WriteLn('Database to Markdown Documentation Generator');
  WriteLn;
  WriteLn('Usage:');
  WriteLn('  dbtomd <type> <connection_params> [output_file]');
  WriteLn;
  WriteLn('Database Types:');
  WriteLn('  mysql      - MySQL/MariaDB database');
  WriteLn('  postgresql - PostgreSQL database');
  WriteLn('  sqlite     - SQLite database file');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  dbtomd mysql host=localhost db=mydb user=root pass=secret');
  WriteLn('  dbtomd postgresql host=localhost db=mydb user=postgres pass=secret');
  WriteLn('  dbtomd sqlite db=/path/to/database.db');
  WriteLn('  dbtomd mysql host=localhost db=mydb user=root pass=secret output.md');
end;

function ParseConnectionParams(const ConnStr: string): TStringList;
var
  Params: TStringList;
  i: Integer;
  Param, Key, Value: string;
begin
  Result := TStringList.Create;
  Params := TStringList.Create;
  try
    Params.Delimiter := ' ';
    Params.StrictDelimiter := True;
    Params.DelimitedText := ConnStr;

    for i := 0 to Params.Count - 1 do
    begin
      Param := Params[i];
      if Pos('=', Param) > 0 then
      begin
        Key := Copy(Param, 1, Pos('=', Param) - 1);
        Value := Copy(Param, Pos('=', Param) + 1, Length(Param));
        Result.Values[Key] := Value;
      end;
    end;
  finally
    Params.Free;
  end;
end;

procedure WriteMarkdownHeader(var F: TextFile; Describer: IDBDescriber);
begin
  WriteLn(F, '# Database Documentation: ', Describer.GetDatabaseName);
  WriteLn(F);
  WriteLn(F, 'Database Type: ', Describer.GetTypeName);
  WriteLn(F);
  WriteLn(F, 'Generated: ', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  WriteLn(F);
end;

procedure DescribeTable(var F: TextFile; Describer: IDBDescriber; const TableName: string);
var
  Query: TSQLQuery;
begin
  WriteLn(F, '## Table: `', TableName, '`');
  WriteLn(F);
  WriteLn(F, '| Column | Type | Nullable | Default | Key |');
  WriteLn(F, '|--------|------|----------|---------|-----|');

  Query := TSQLQuery.Create(nil);
  try
    Query.Database := Describer.GetConnection;
    Query.SQL.Text := Describer.GetColumnInfoSQL(TableName);
    Query.Open;

    while not Query.EOF do
    begin
      Describer.WriteColumnRow(F, Query);
      Query.Next;
    end;

    Query.Close;
  finally
    Query.Free;
  end;

  WriteLn(F);
end;

procedure GenerateMarkdown(Describer: IDBDescriber; const OutputFile: string);
var
  F: TextFile;
  Query: TSQLQuery;
  Tables: TStringList;
  i: Integer;
begin
  AssignFile(F, OutputFile);
  Rewrite(F);

  try
    WriteMarkdownHeader(F, Describer);

    Tables := TStringList.Create;
    Query := TSQLQuery.Create(nil);
    try
      Query.Database := Describer.GetConnection;
      Query.SQL.Text := Describer.GetTableListSQL;
      Query.Open;

      while not Query.EOF do
      begin
        Tables.Add(Query.Fields[0].AsString);
        Query.Next;
      end;

      Query.Close;

      WriteLn(F, '## Tables');
      WriteLn(F);
      for i := 0 to Tables.Count - 1 do
        WriteLn(F, '- [', Tables[i], '](#table-', LowerCase(Tables[i]), ')');
      WriteLn(F);

      for i := 0 to Tables.Count - 1 do
        DescribeTable(F, Describer, Tables[i]);

    finally
      Query.Free;
      Tables.Free;
    end;

  finally
    CloseFile(F);
  end;

  WriteLn('Markdown documentation generated: ', OutputFile);
end;

var
  DBTypeStr, ConnStr, OutputFile: string;
  Describer: IDBDescriber;
  Params: TStringList;
  i: Integer;

begin
  if ParamCount < 2 then
  begin
    ShowUsage;
    Exit;
  end;

  DBTypeStr := LowerCase(ParamStr(1));

  Describer := CreateDescriber(DBTypeStr);
  if Describer = nil then
  begin
    WriteLn('Error: Invalid database type');
    ShowUsage;
    Exit;
  end;

  ConnStr := '';
  OutputFile := '';

  for i := 2 to ParamCount do
  begin
    if Pos('=', ParamStr(i)) > 0 then
      ConnStr := ConnStr + ParamStr(i) + ' '
    else
      OutputFile := ParamStr(i);
  end;

  if OutputFile = '' then
    OutputFile := 'database_doc.md';

  Params := ParseConnectionParams(Trim(ConnStr));
  try
    if Params.Values['db'] = '' then
    begin
      WriteLn('Error: Database parameter is required');
      ShowUsage;
      Exit;
    end;

    try
      Describer.Connect(Params);
      WriteLn('Connected to ', Describer.GetTypeName, ' database: ', Describer.GetDatabaseName);
      GenerateMarkdown(Describer, OutputFile);
    finally
      Describer.Disconnect;
    end;

  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;

  Params.Free;
end.

