unit unitHttpClient;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, openssl, opensslsockets;

function HttpGetRequest(const S: string): TStringStream;

implementation

function HttpGetRequest(const S: string): TStringStream;
var
  LURL: string;
  Client: TFPHttpClient;
begin
  LURL := S;
  Result := TStringStream.Create;
  if LURL.StartsWith('//') then
    LURL := 'https:' + LURL;
  Client := TFPHttpClient.Create(Nil);
  try
    Client.Get(LURL, Result);
    Result.Seek(0, soBeginning);
  finally
    Client.Free;
  end;
end;
end.

