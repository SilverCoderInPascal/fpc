unit uWebPageScanner;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  fphttpclient,
  fasthtmlparser,
  htmtool,
  HtmUtils,
  URIParser,
  openssl,
  opensslsockets,
  Contnrs,
  HTTPDefs,
  uWebpagelinks;

type
  TWebPageScanner = class
  private
    FClient: TFPHTTPClient;
    FProcessedLinks: TStringList;
    function CheckThisLink(const ALink: string): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Scan(const URL: string; APageList: TFPObjectList);
  end;

implementation

constructor TWebPageScanner.Create;
begin
  inherited Create;
  FClient := TFPHTTPClient.Create(nil);

  FProcessedLinks := TStringList.Create;
  FProcessedLinks.Sorted := True;
  FProcessedLinks.Duplicates := dupIgnore;
end;

destructor TWebPageScanner.Destroy;
begin
  FProcessedLinks.Free;
  FClient.Free;
  inherited Destroy;
end;

function TWebPageScanner.CheckThisLink(const ALink: string): Integer;
var
  N: SizeInt;
  BasePage: String;
  LHeaders: TStringList;
begin
  //WriteLn('CheckThisLink::ALink ', ALink);
  BasePage := GetBaseURL(ALink);
  N := FProcessedLinks.IndexOf(BasePage);
  if N >= 0 then
  begin
    //WriteLn('BasePage ', BasePage, ' found at position ', N);
    Result := PtrInt(FProcessedLinks.Objects[N]);
    Exit;
  end;

  Result := -1;
  LHeaders := TStringList.Create;
  try
    try
      FClient.Head(ALink, LHeaders);
      Result := 200; // Optionally use FClient.ResponseStatusCode
    except
      on E: EHTTP do
        Result := E.StatusCode;
    end;
  finally
    LHeaders.Free;
  end;

  FProcessedLinks.AddObject(BasePage, TObject(PtrInt(Result)));
end;

procedure TWebPageScanner.Scan(const URL: string; APageList: TFPObjectList);
var
  PageStream: TStringStream;
  Parser: TWebPageLinks;
  RawLink, FullLink: string;
  LStatus: Integer;
begin
  PageStream := TStringStream.Create('');
  try
    WriteLn('Downloading: ', URL);
    FClient.Get(URL, PageStream);
    PageStream.Position := 0;

    Parser := TWebPageLinks.Create(PageStream);
    try
      if Parser.Parse then
      begin
        for RawLink in Parser.Links do
        begin
          //WriteLn('RawLink: ', RawLink);
          FullLink := NormalizeURL(URL, RawLink);
          LStatus := CheckThisLink(FullLink);
          APageList.Add(TWebPageStatus.Create(FullLink, LStatus));
        end;
      end;
    finally
      Parser.Free;
    end;
  finally
    PageStream.Free;
  end;
end;


end.

