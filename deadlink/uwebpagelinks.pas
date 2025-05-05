unit uWebpagelinks;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  fasthtmlparser,
  htmtool,
  HtmUtils,
  URIParser,
  Contnrs,
  HTTPDefs;


type
  { TWebPageStatus }

  TWebPageStatus = class
  public
    FLink: string;
    FStatus: Integer;

    constructor Create(const ALink: string; AStatus: Integer);
  end;

  { TWebPageLinks }

  TWebPageLinks = class
    FStream: TStringStream;
    procedure DoOnFoundTag(NoCaseTag, ActualTag: string);
    procedure DoOnFoundText(Text: string);
  private
    FLinks: TStringList;
  public
    constructor Create(AStream: TStringStream);
    destructor Destroy; override;
    function Parse(AStream: TStringStream = Nil): boolean;
    property Links: TStringList read FLinks;
  end;

function GetBaseURL(const ALink: string): string;
function NormalizeURL(const BaseURL, RelativeURL: string): string;

implementation

function GetBaseURL(const ALink: string): string;
var
  N: SizeInt;
begin
  N := Pos('#', ALink);
  if N > 0 then
    Result := Copy(ALink, 1, N-1)
  else
    Result := ALink;
end;

function NormalizeURL(const BaseURL, RelativeURL: string): string;
var
  BaseURI: TURI;
begin
  if Pos('http', RelativeURL) = 1 then
    Exit(RelativeURL); // Already absolute
  if Copy(RelativeURL, 1, 2) = '//' then
  begin
    // Protocol-relative (e.g., //example.com)
    if Pos('https://', BaseURL) = 1 then
      Exit('https:' + RelativeURL)
    else
      Exit('http:' + RelativeURL);
  end;

  ResolveRelativeURI(BaseURL, RelativeURL, Result);
end;

{ TWebPageStatus }

constructor TWebPageStatus.Create(const ALink: string; AStatus: Integer);
begin
  FLink := ALink;
  FStatus := AStatus;
end;

{ TWebPageLinks }

procedure TWebPageLinks.DoOnFoundTag(NoCaseTag, ActualTag: string);
var
  LTag, LAttr, LLink: String;
begin
  //WriteLn('ActualTag=', ActualTag);
  if IsTag('a', ActualTag) then
  begin
    LTag := CleanHtm1(ActualTag);
    LLink := GetVal(LTag, 'href');
    if LLink <> '' then
      FLinks.Add(LLink);
  end;
end;

procedure TWebPageLinks.DoOnFoundText(Text: string);
begin
  //WriteLn('Text=', Text);
end;

constructor TWebPageLinks.Create(AStream: TStringStream);
begin
  FStream := AStream;
  FLinks := TStringlist.Create;
end;

destructor TWebPageLinks.Destroy;
begin
  FLinks.Free;
  inherited Destroy;
end;

function TWebPageLinks.Parse(AStream: TStringStream): boolean;
var
  LStream: TStringStream;
  LParser: THtmlParser;
begin
  Result := True;
  LStream := FStream;
  if Assigned(AStream) then LStream := AStream;

  FLinks.Clear;
  LParser := THtmlParser.Create(LStream.DataString);
  try
    LParser.OnFoundTag:= @DoOnFoundTag;
    LParser.OnFoundText:= @DoOnFoundText;
    LParser.Exec;
  finally
    LParser.Free;
  end;
  Result := FLinks.Count > 0;
end;

end.

