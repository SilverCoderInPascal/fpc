program deadlinkc;

{
https://github.com/z505/fast-html-parser/tree/master

TO DO:
Put onto github
}

uses
  SysUtils,
  Classes,
  Contnrs,
  uWebpagelinks,
  uWebPageScanner;


procedure DumpPageList(const URL: string; AList: TFPObjectList);
var
  I: Integer;
  LinksFailed: integer = 0;
  WPS: TWebPageStatus;
begin
  WriteLn('Result from Page - ', URL);
  for I := 0 to AList.Count-1 do
  begin
    WPS := TWebPageStatus(AList.Items[I]);
    if WPS.FStatus = 200 then
      WriteLn('[OK]     ', WPS.FLink)
    else
    begin
      Inc(LinksFailed);
      WriteLn('[Failed] ', WPS.FLink);
    end;
  end;
  WriteLn;
  WriteLn('Number of bad links: ', LinksFailed);
end;

procedure Main(const URL: string);
var
  Scanner: TWebPageScanner;
  PageList: TFPObjectList;
begin
  PageList:= TFPObjectList.Create(true);
  Scanner := TWebPageScanner.Create;
  try
    Scanner.Scan(URL, PageList);
    DumpPageList(URL, PageList);
  finally
    Scanner.Free;
    PageList.Free;
  end;
end;


begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: DeadLinkChecker <URL>');
    Halt(1);
  end;
  //Main('https://www.freepascal.org/docs-html/current/ref/refse8.html#x19-180001.8');
  Main(ParamStr(1));
end.

