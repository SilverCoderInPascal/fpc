unit uDBDescriber;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, sqldb;

type
  { Interface for database operations }
  IDBDescriber = interface
    ['{8F5E3A21-4B2C-4D5F-9E7A-2C1B3D4E5F6A}']
    function GetTypeName: string;
    procedure Connect(const Params: TStringList);
    procedure Disconnect;
    function GetTableListSQL: string;
    function GetColumnInfoSQL(const TableName: string): string;
    procedure WriteColumnRow(var F: TextFile; Query: TSQLQuery);
    function IsConnected: Boolean;
    function GetConnection: TSQLConnection;
    function GetDatabaseName: string;
  end;

  { Base abstract class for database describers }
  TBaseDBDescriber = class(TInterfacedObject, IDBDescriber)
  protected
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    FDatabaseName: string;
    procedure CreateConnection; virtual; abstract;
    procedure SetConnectionParams(const Params: TStringList); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function GetTypeName: string; virtual; abstract;
    procedure Connect(const Params: TStringList);
    procedure Disconnect;
    function GetTableListSQL: string; virtual; abstract;
    function GetColumnInfoSQL(const TableName: string): string; virtual; abstract;
    procedure WriteColumnRow(var F: TextFile; Query: TSQLQuery); virtual; abstract;
    function IsConnected: Boolean;
    function GetConnection: TSQLConnection;
    function GetDatabaseName: string;
  end;

implementation

{ TBaseDBDescriber Implementation }

constructor TBaseDBDescriber.Create;
begin
  inherited Create;
  FTransaction := TSQLTransaction.Create(nil);
  CreateConnection;
  FConnection.Transaction := FTransaction;
  FTransaction.Database := FConnection;
end;

destructor TBaseDBDescriber.Destroy;
begin
  Disconnect;
  FConnection.Free;
  FTransaction.Free;
  inherited Destroy;
end;

procedure TBaseDBDescriber.Connect(const Params: TStringList);
begin
  SetConnectionParams(Params);
  FConnection.Open;
end;

procedure TBaseDBDescriber.Disconnect;
begin
  if FConnection.Connected then
    FConnection.Close;
end;

function TBaseDBDescriber.IsConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TBaseDBDescriber.GetConnection: TSQLConnection;
begin
  Result := FConnection;
end;

function TBaseDBDescriber.GetDatabaseName: string;
begin
  Result := FDatabaseName;
end;

end.

