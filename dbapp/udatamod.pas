unit udatamod;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DB;

type

  { TCarsDataMod }

  TCarsDataMod = class(TDataModule)
    DataSource1: TDataSource;
    qryPermitList: TSQLQuery;
    SQLite3Connection1: TSQLite3Connection;
    qrySqlCmd: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure DataModuleDestroy(Sender: TObject);
  private
    function NextPermitId: integer;
  public
    procedure Open;
    procedure AddPermit(ARego: string; AVehicle, AColor: string);
    procedure FilterPermitsByRego(ARego: string);
    procedure ResetPermitsView;
    function IsValidPermit(ARego: string): boolean;
  end;

var
  CarsDataMod: TCarsDataMod;

implementation

{$R *.lfm}

{
source code from here: https://wiki.freepascal.org/SQLite
}
procedure RefreshADatasetAfterInsert(pDataSet: TSQLQuery);
//This procedure refreshes a dataset and positions cursor to last record
//To be used only if DataSet is guaranteed to be sorted by an autoincrement primary key
var
  vLastID: Integer;
  vUpdateStatus : TUpdateStatus;
begin
  vUpdateStatus := pDataset.UpdateStatus;
  pDataset.ApplyUpdates;
  vLastID:=(pDataSet.DataBase as TSQLite3Connection).GetInsertID;
  // the refresh will not happen because update status <> inserted!!
  //if vUpdateStatus = usInserted then
  begin
    pDataset.Refresh;
    //Dangerous!
    pDataSet.Last;
  end;
end;

{ TCarsDataMod }

procedure TCarsDataMod.DataModuleDestroy(Sender: TObject);
begin
  if SQLite3Connection1.Connected then
    SQLite3Connection1.Close();
end;

function TCarsDataMod.NextPermitId: integer;
var
  qryTemp: TSQLQuery;
begin
  qryTemp := TSQLQuery.Create(nil);
  try
    qryTemp.SQLConnection := SQLite3Connection1;
    qryTemp.Transaction := SQLTransaction1;
    qryTemp.SQL.Text := 'select max(permit_id) from permit';
    qryTemp.Open;
    Result := qryTemp.Fields[0].AsInteger + 1;
  finally
    qryTemp.Free;
  end;
end;

procedure TCarsDataMod.Open;
begin
  SQLite3Connection1.Connected:= True;
  qryPermitList.Open;
end;

procedure TCarsDataMod.AddPermit(ARego: string; AVehicle, AColor: string);
const
  Year = 2024;
  SqlInsertStmt =
    'insert into permit values(' +
    ':permit_id, :rego, :year, :vehicle_info, :vehicle_color)';
var
  Permit_Id: integer;
begin
  SQLTransaction1.StartTransaction;
  try
    qrySqlCmd.SQL.Text := SqlInsertStmt;
    qrySqlCmd.ParamByName('permit_id').AsInteger := NextPermitId;
    qrySqlCmd.ParamByName('rego').AsString := ARego;
    qrySqlCmd.ParamByName('year').AsInteger := Year;
    qrySqlCmd.ParamByName('vehicle_info').AsString:= AVehicle;
    qrySqlCmd.ParamByName('vehicle_color').AsString:= AColor;

    qrySqlCmd.ExecSQL;
    SQLTransaction1.Commit;

    RefreshADatasetAfterInsert(qryPermitList);
  except
    on e: EDatabaseError do
    begin
      SQLTransaction1.Rollback;
      raise;
    end;
    on e: Exception do
      raise;
  end;
end;

procedure TCarsDataMod.FilterPermitsByRego(ARego: string);
begin
  qryPermitList.Filter:= 'Rego=' + QuotedStr(ARego);
  qryPermitList.Filtered := True;
  {
  qryPermitList.Close;
  qryPermitList.SQL.Text :=
    'select * from permit where rego = ' + QuotedStr(ARego);
  qryPermitList.Open;
  }
end;

procedure TCarsDataMod.ResetPermitsView;
begin
  qryPermitList.Filter:= '';
  qryPermitList.Filtered := False;

  {
  qryPermitList.Close;
  qryPermitList.SQL.Text := 'select * from permit';
  qryPermitList.Open;
  }
end;

function TCarsDataMod.IsValidPermit(ARego: string): boolean;
var
  qryTemp: TSQLQuery;
begin
  qryTemp := TSQLQuery.Create(nil);
  try
    qryTemp.SQLConnection := SQLite3Connection1;
    qryTemp.Transaction := SQLTransaction1;
    qryTemp.SQL.Text :=
      'select 1 from permit where rego = ' + QuotedStr(ARego);
    qryTemp.Open;
    Result := not qryTemp.IsEmpty;
  finally
    qryTemp.Free;
  end;
end;

end.

