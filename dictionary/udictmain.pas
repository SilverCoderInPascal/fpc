unit uDictMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAdd: TButton;
    btnDelete: TButton;
    edtDefinition: TEdit;
    edtWord: TEdit;
    lbWords: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure edtDefinitionChange(Sender: TObject);
    procedure edtWordChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbWordsSelectionChange(Sender: TObject; User: boolean);
  private
    FDict: TStringlist;
    procedure FillListBox;
    function IsCompleteEntry: boolean;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  DictionaryFile = 'dictionary.txt';

{ TForm1 }

procedure TForm1.btnAddClick(Sender: TObject);
var
  LPos: Integer;
begin
  LPos := FDict.IndexOfName(edtWord.Text);
  if LPos <> -1 then
  begin
    if MessageDlg('Word found',
      'Word already exists in dictionary. Update?',
      mtConfirmation, [mbYes, mbNo], '') = mrNo then
      Exit;
      FDict.Delete(LPos);
  end;

  {this will update or add to the dictionary}
  FDict.Values[edtWord.Text] := edtDefinition.Text;
  FillListBox;
  edtWord.Text := '';
  edtDefinition.Text := '';
end;

procedure TForm1.btnDeleteClick(Sender: TObject);
var
  I, LIdx: Integer;
begin
  for I := 0 to lbWords.Items.Count-1 do
  begin
    if lbWords.Selected[I] then
    begin
      LIdx := FDict.IndexOfName(lbWords.Items[I]);
      if LIdx >= 0 then
        FDict.Delete(LIdx);
    end;
  end;
  FillListBox;
end;

procedure TForm1.edtDefinitionChange(Sender: TObject);
begin
  btnAdd.Enabled := IsCompleteEntry;
end;

procedure TForm1.edtWordChange(Sender: TObject);
begin
  btnAdd.Enabled := IsCompleteEntry;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  btnAdd.Enabled := False;

  FDict := TStringlist.Create;
  FDict.Sorted := True;
  if FileExists(DictionaryFile) then
    FDict.LoadFromFile(DictionaryFile);
  FillListBox;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDict.SaveToFile(DictionaryFile);
end;

procedure TForm1.lbWordsSelectionChange(Sender: TObject; User: boolean);
var
  LWord: String;
begin
  LWord := lbWords.Items[lbWords.ItemIndex];
  edtWord.Text := LWord;
  edtDefinition.Text := FDict.Values[LWord];
end;

procedure TForm1.FillListBox;
var
  I: Integer;
begin
  lbWords.Clear;
  for I := 0 to FDict.Count-1 do
  begin
    lbWords.Items.Add(FDict.Names[I]);
  end;
end;

function TForm1.IsCompleteEntry: boolean;
begin
  Result := (edtWord.Text <> '') and (edtDefinition.Text <> '');
end;

end.

