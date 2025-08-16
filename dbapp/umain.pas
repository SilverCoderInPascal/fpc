unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DBCtrls,
  DBGrids, Grids, DB;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnInsert: TButton;
    btnFind: TButton;
    btnResetView: TButton;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Edit1: TEdit;
    Label1: TLabel;
    StringGrid1: TStringGrid;
    procedure btnInsertClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnResetViewClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses uDataMod, Math;

{functions for testing ...}

function RandomAlphaRange(const a,b:Char):Char;
begin
  Result := chr(math.RandomRange(Ord(a),Ord(b)));
end;

function RandomRego: string;

  function DummyRego: string;
  var
    I: Integer;
    Letters: string = '';
    Numbers: string = '';
  begin
    Randomize;
    for I := 1 to 3 do
    begin
      Letters := Letters + RandomAlphaRange('A', 'Z');
      Numbers := Numbers + RandomRange(0, 9).ToString;
    end;
    Result := Letters + Numbers;
  end;

var
  Rego: string;
begin
  while True do
  begin
    Rego := DummyRego;
    if not CarsDataMod.IsValidPermit(Rego) then
    begin
      Result := Rego;
      Exit;
    end;
  end;
end;

function RandomColor: string;
const
  Colors: array[1..9] of string = (
    'RED', 'ORANGE', 'YELLOW', 'GREEN', 'BLUE', 'INDIGO',
    'VIOLET', 'BLACK', 'WHITE');
begin
  Randomize;
  Result := Colors[RandomRange(1, 9)];
end;

{ TForm1 }

procedure TForm1.btnInsertClick(Sender: TObject);
begin
  CarsDataMod.AddPermit(RandomRego, 'KIA RIO', RandomColor);
end;

procedure TForm1.btnFindClick(Sender: TObject);
begin
  CarsDataMod.FilterPermitsByRego(edit1.text);
  {
  if not CarsDataMod.qryPermitList.Locate('rego',
    edit1.text, [loCaseInsensitive]) then
  begin
    ShowMessage('Record not found');
  end;
  }
end;

procedure TForm1.btnResetViewClick(Sender: TObject);
begin
  CarsDataMod.ResetPermitsView;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  CarsDataMod.Open;
end;

end.

