unit dictionary_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls,SysUtils, Forms, Controls, Graphics, Dialogs;

type
  TEditMode = (emNone, emAdd, emUpdate);

type
  TForm1 = class(TForm)
    btnUpdate:TButton;
    btnLoad:TButton;
    btnSave:TButton;
    btnSaveAs:TButton;
    btnClear:TButton;
    btnDelete:TButton;
    btnAdd:TButton;
    btnSaveEntry:TButton;
    btnFind:TButton;
    edtKey:TEdit;
    edtFind:TEdit;
    Label1:TLabel;
    Label2:TLabel;
    Label3:TLabel;
    Label4:TLabel;
    lboxKeys:TListBox;
    memValue:TMemo;
    OpenDialog1:TOpenDialog;
    SaveDialog1:TSaveDialog;
    procedure btnAddClick(Sender:TObject);
    procedure btnFindClick(Sender:TObject);
    procedure btnLoadClick(Sender:TObject);
    procedure btnSaveAsClick(Sender:TObject);
    procedure btnSaveClick(Sender:TObject);
    procedure btnClearClick(Sender:TObject);
    procedure btnDeleteClick(Sender:TObject);
    procedure btnSaveEntryClick(Sender:TObject);
    procedure btnUpdateClick(Sender:TObject);
    procedure FormCreate(Sender:TObject);
    procedure FormDestroy(Sender:TObject);
    procedure lboxKeysSelectionChange(Sender:TObject;User:boolean);
  private
    FEditMode: TEditMode;
    FDictionaryName: string;
    FDictionaryItems: TStringList;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender:TObject);
begin
  edtKey.Text := '';
  memValue.Lines.Text := '';
  edtFind.Text := '';
  btnSaveEntry.Enabled := False;

  FEditMode := emNone;
  FDictionaryName := '';
  FDictionaryItems := TStringList.Create;
end;

procedure TForm1.btnLoadClick(Sender:TObject);
var
  I:Integer;
  LKey:String;
begin
  if OpenDialog1.Execute then
  begin
    lboxKeys.Items.Clear;

    FDictionaryName := OpenDialog1.FileName;
    FDictionaryItems.LoadFromFile(FDictionaryName);
    for I := 0 to Pred(FDictionaryItems.Count) do
    begin
      LKey := FDictionaryItems.Names[I];
      lboxKeys.Items.Add(LKey);
    end;
  end;
end;

procedure TForm1.btnAddClick(Sender:TObject);
begin
  FEditMode := emAdd;
  edtKey.Text := '';
  memValue.Lines.Text := '';
  btnSaveEntry.Enabled := True;
  edtKey.SetFocus;
end;

procedure TForm1.btnFindClick(Sender:TObject);
var
  FIndex:Integer;
begin
  FIndex := lboxKeys.Items.IndexOf(edtFind.Text);
  if FIndex >= 0 then
    lboxKeys.ItemIndex := FIndex;
end;

procedure TForm1.btnSaveAsClick(Sender:TObject);
begin
  if SaveDialog1.Execute then
  begin
    FDictionaryName := SaveDialog1.FileName;
    FDictionaryItems.SaveToFile(FDictionaryName);
  end;
end;

procedure TForm1.btnSaveClick(Sender:TObject);
begin
  if FDictionaryName <> '' then
    FDictionaryItems.SaveToFile(FDictionaryName)
  else
    btnSaveAsClick(Sender)
end;

procedure TForm1.btnClearClick(Sender:TObject);
begin
  FDictionaryItems.Clear;
  lboxKeys.Items.Clear;
end;

procedure TForm1.btnDeleteClick(Sender:TObject);
var
  I:Integer;
  LKey:String;
  LPos:Integer;
begin
  for I := Pred(lboxKeys.Items.Count) downto 0 do
  begin
    if lboxKeys.Selected[I] then
    begin
      LKey := lboxKeys.Items[I];
      LPos := FDictionaryItems.IndexOfName(LKey);
      if LPos >= 0 then
      begin
        FDictionaryItems.Delete(LPos);
        lboxKeys.Items.Delete(I);
      end;
    end;
  end;
end;

procedure TForm1.btnSaveEntryClick(Sender:TObject);
var
  LKey:TCaption;
  LValue:String;
begin
  LKey := edtKey.Text;
  LValue := memValue.Lines.Text;

  FDictionaryItems.Values[LKey] := LValue;

  if FEditMode = emAdd then
    lboxKeys.Items.Add(LKey);

  FEditMode := emNone;
  btnSaveEntry.Enabled := False;
end;

procedure TForm1.btnUpdateClick(Sender:TObject);
begin
  FEditMode := emUpdate;
  {fields updated by selecting item in listbox.
   and lazy here also in not make fields r/o }
  btnSaveEntry.Enabled := True;
  memValue.SetFocus;
end;

procedure TForm1.FormDestroy(Sender:TObject);
begin
  FDictionaryItems.Free;
end;

procedure TForm1.lboxKeysSelectionChange(Sender:TObject;User:boolean);
var
  LKey:String;
  LValue:String;
begin
  LKey := lboxKeys.Items[lboxKeys.ItemIndex];
  LValue := FDictionaryItems.Values[LKey];

  edtKey.Text := LKey;
  memValue.Lines.Text := LValue;
end;


end.

