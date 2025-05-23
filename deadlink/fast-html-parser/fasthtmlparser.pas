{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    FastHTMLParser unit to parse HTML
                  (disect html into its tags and text.)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 Original AUTHOR       : James Azarja, http://www.jazarsoft.com/

 CONTRIBUTORS : L505, http://z505.com

 NOTES: Modified by Lars to work with both freepascal and delphi, and contains
        more features

 LEGAL        : Copyright (C) 2004 Jazarsoft, All Rights Reserved.
                Modified 2005 Lars (L505)
--------------------------------------------------------------------------------
 LICENSE/TERMS
--------------------------------------------------------------------------------

 This code may be used and modified by anyone so long as  this header and
 copyright  information remains intact.

 The code is provided "AS-IS" and without WARRANTY OF ANY KIND,
 expressed, implied or otherwise, including and without limitation, any
 warranty of merchantability or fitness for a  particular purpose.

 In no event shall the author be liable for any special, incidental,
 indirect or consequential damages whatsoever (including, without
 limitation, damages for loss of profits, business interruption, loss
 of information, or any other loss), whether or not advised of the
 possibility of damage, and on any theory of liability, arising out of
 or in connection with the use or inability to use this software.
}


{$ifdef fpc} {$MODE OBJFPC} {$H+}{$endif}

// {$DEFINE DEBUGLN_ON}

unit fasthtmlparser;


interface

uses
{$IFDEF KOL_MCK}
  KOL,
{$else}
  SysUtils,
{$ENDIF}
  //Dialogs, //DEBUG REMOVE THIS LATER
  htmtool, htmutils;


{$IFDEF DEBUGLN_ON}
  // dummy, default debugging
  procedure debugproc(s: string);
	// for custom debugging, assign this in your units
  var debugln: procedure(s: string) = debugproc;
{$ENDIF}

type

  // when tag content found in HTML, including names and values
  // case insensitive analysis available via NoCaseTag
  TOnFoundTag = procedure(NoCaseTag, ActualTag: string) of object;
  // procedural:
  TOnFoundTagP = procedure(NoCaseTag, ActualTag: string);

  // when text  found in the HTML
  TOnFoundText = procedure(Text: string) of object;
  // procedural:
  TOnFoundTextP = procedure(Text: string);

  // Lars's modified html parser, case insensitive or case sensitive
  THTMLParser = class(TObject)
  private
    FElementFound: boolean;
    FElementTag: string;
    FElementTagEnd: string;
    FElementHtml: string;
    FElementName: string;
    FElementId: string;
    FFindingElementName: boolean;
    FFindingElementId: boolean;
    procedure NilOnFoundTag(NoCaseTag, ActualTag: string);
    procedure NilOnFoundText(Text: string);
    procedure ElementOnFoundTag(NoCaseTag, ActualTag: string);
    procedure ElementOnFoundText(Text: string);
  public
    UseTagTextArray: boolean;
    OnFoundTag: TOnFoundTag;
    OnFoundText: TOnFoundText;
    OnFoundTagP: TOnFoundTagP;
    OnFoundTextP: TOnFoundTextP;
    Raw: Pchar;
    constructor Create(sRaw: string);overload;
    constructor Create(pRaw: PChar);overload;
    procedure Exec;
    function GetElementByName(name: string; var Tag: string; var TagEnd: string): string;
    function GetElementById(id: string; var Tag: string; var TagEnd: string): string;
  end;


implementation


// default debugging, do nothing, let user do his own by assigning DebugLn var
procedure debugproc(s: string);
begin
end;

function CopyBuffer(StartIndex: PChar; Length: Integer): string;
var
  S: string;
begin
  SetLength(S, Length);
  StrLCopy(@S[1], StartIndex, Length);
  Result:= S;
end;

{ ************************ THTMLParser ************************************** }

constructor THTMLParser.Create(pRaw: Pchar);
begin
  if pRaw = '' then exit;
  if pRaw = nil then exit;
  Raw:= pRaw;
  FElementFound := false;
  FElementTag := '';
  FElementTagEnd := '';
  FElementHtml := '';
  FElementName := '';
  FElementId := '';
  FFindingElementName := false;
  FFindingElementId := false;
end;

constructor THTMLParser.Create(sRaw: string);
begin
  if sRaw = '' then exit;
  Raw:= Pchar(sRaw);
end;

{ default dummy "do nothing" class events if unassigned }
procedure THTMLParser.NilOnFoundTag(NoCaseTag, ActualTag: string);
begin
end;

procedure THTMLParser.NilOnFoundText(Text: string);
begin
end;

procedure THTMLParser.ElementOnFoundTag(NoCaseTag, ActualTag: string);
begin
  // tags inside
  if FElementFound then FElementHtml := FElementHtml + ActualTag;

  if (FElementId <> '') and (FFindingElementName) then begin
    if GetVal(ActualTag, 'name') = FElementName then begin
      FElementFound := true;
      FElementTag := ActualTag;
      // FElementName := '';
    end;
  end;

  if (FElementId <> '') and (FFindingElementId) then begin
    if GetVal(ActualTag, 'id') = FElementId then begin
      FElementFound := true;
      FElementTag := ActualTag;
      // FElementId := '';
    end;
  end;
  // closer tag
  if NoCaseTag[2] = '/' then FElementFound := false;
end;

procedure THTMLParser.ElementOnFoundText(Text: string);
begin
  if FElementFound then begin
    FElementHtml := FElementHtml + Text;
  end;
end;

{ default dummy "do nothing" procedural events if unassigned }
procedure NilOnFoundTagP(NoCaseTag, ActualTag: string);
begin
end;

procedure NilOnFoundTextP(Text: string);
begin
end;

procedure THTMLParser.Exec;
var
  L, TL, I: Integer;
  Done: Boolean;
  TagStart, TextStart, P: PChar;   // Pointer to current char.
  C: Char;
begin
  {$IFDEF DEBUGLN_ON}debugln('FastHtmlParser Exec Begin');{$ENDIF}
  { set nil events once rather than checking for nil each time tag is found }
  if not assigned(OnFoundText) then  OnFoundText:= @NilOnFoundText;
  if not assigned(OnFoundTag) then OnFoundTag:= @NilOnFoundTag;
  if not assigned(OnFoundTextP) then  OnFoundTextP:= @NilOnFoundTextP;
  if not assigned(OnFoundTagP) then OnFoundTagP:= @NilOnFoundTagP;

  TL:= StrLen(Raw);
  I:= 0;
  P:= Raw;
  Done:= False;
  if P <> nil then
  begin
    TagStart:= nil;
    repeat
      TextStart:= P;
      { Get next tag position }
      while Not (P^ in [ '<', #0 ]) do
      begin
        Inc(P); Inc(I);
        if I >= TL then
        begin
          Done:= True;
          Break;
        end;
      end;
      if Done then Break;

      { Is there any text before ? }
      if (TextStart <> nil) and (P > TextStart) then
      begin
        L:= P - TextStart;
        { Yes, copy to buffer, OO event:}
        OnFoundText( CopyBuffer(TextStart, L) );
        // procedural:
        OnFoundTextP( CopyBuffer(TextStart, L) );
      end else
      begin
        TextStart:= nil;
      end;
      { No }

      TagStart:= P;
      while Not (P^ in [ '>', #0]) do
      begin
        // Find string in tag
        if (P^ = '"') or (P^ = '''') then
        begin
          C:= P^;
          Inc(P); Inc(I); // Skip current char " or '

          // Skip until string end
          while Not (P^ in [C, #0]) do
          begin
            Inc(P);Inc(I);
          end;
        end;

        Inc(P);Inc(I);
        if I >= TL then
        begin
          Done:= True;
          Break;
        end;
      end;
      if Done then Break;

      { Copy this tag to buffer }
      L:= P - TagStart + 1;
      // OO event
      OnFoundTag(uppercase(CopyBuffer(TagStart, L)), CopyBuffer(TagStart, L ) ); //L505: added uppercase
      // procedural
      OnFoundTagP(uppercase(CopyBuffer(TagStart, L)), CopyBuffer(TagStart, L ) );

      Inc(P); Inc(I);
      if I >= TL then Break;
    until (Done);
  end;
  {$IFDEF DEBUGLN_ON}debugln('FastHtmlParser Exec End');{$ENDIF}
end;

function THTMLParser.GetElementByName(name: string; var Tag: string; var TagEnd: string): string;
begin
  result := '';
  FFindingElementName := true;
  OnFoundTag := @ElementOnFoundTag;
  OnFoundText := @ElementOnFoundText;
  FElementName := name;
  Exec;
  OnFoundTag := @NilOnFoundTag;
  OnFoundText := @NilOnFoundText;
  Tag := FElementTag;
  TagEnd := FElementTagEnd;
  result := FElementHtml;
  FFindingElementName := false;
  FElementTag := '';
  FElementTagEnd := '';
  FElementHtml := '';
  FElementId := '';
  FElementName := '';
end;

function THTMLParser.GetElementById(id: string; var Tag: string; var TagEnd: string): string;
begin
  result := '';
  FFindingElementId := true;
  OnFoundTag := @ElementOnFoundTag;
  OnFoundText := @ElementOnFoundText;
  FElementId := id;
  Exec;
  OnFoundTag := @NilOnFoundTag;
  OnFoundText := @NilOnFoundText;
  Tag := FElementTag;
  TagEnd := FElementTagEnd;
  result := FElementHtml;
  FFindingElementId := false;
  FElementTag := '';
  FElementTagEnd := '';
  FElementHtml := '';
  FElementId := '';
  FElementName := '';
end;

end.

