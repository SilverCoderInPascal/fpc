unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  TAGraph, unitWeatherData;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Chart1: TChart;
    Image1: TImage;
    LocationLabel: TLabel;
    OpenDialog1: TOpenDialog;
    TopPanel: TPanel;
    WeatherLabel: TLabel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Panel1Paint(Sender: TObject);
  private
    procedure PrintReport(const AReport: TCurrentData);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LazLogger,
  unitWeatherApi;

{ TForm1 }

const
  clDeepSkyBlue = TColor($FFBF00);

procedure TForm1.PrintReport(const AReport: TCurrentData);
begin
  LocationLabel.Caption := AReport.Location;
  WeatherLabel.Caption := AReport.ConditionText + ' - ' +
                          DegCToStr(AReport.CurrentTempC);
  if Assigned(AReport.WeatherImage) then
    Image1.Picture.LoadFromStream(AReport.WeatherImage);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  LWeatherSource: TWeatherApiApi;
  LWeather: TWeatherOutlook;
begin
  LWeatherSource := TWeatherApiApi.Create;
  try
    LWeather := LWeatherSource.GetCurrentWeather('Brisbane');
    PrintReport(LWeather.Current);
  finally
    LWeather.Free;
    LWeatherSource.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  LWeatherSource: TWeatherApiApi;
  LWeather: TWeatherOutlook;
begin
  LWeatherSource := TWeatherApiApi.Create;
  try
    LWeather := LWeatherSource.GetWeatherForecast('Brisbane', 7);
    if Assigned(LWeather) then
    begin
      //PrintReport(LWeather.Current);
      LWeather.Free;
    end;
  finally
    LWeatherSource.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DebugLogger.LogName := './weatherdata.log';
  DebugLogger.CloseLogFileBetweenWrites := true;

  LocationLabel.Caption := '';
  LocationLabel.Font.Color := clWhite;
  LocationLabel.Font.Style := [fsBold];

  WeatherLabel.Caption := '';
  WeatherLabel.Font.Color := clWhite;
  WeatherLabel.Font.Style := [fsBold];

  TopPanel.Color := clSkyBlue;

  Chart1.Color := clDeepSkyBlue;
  Chart1.BackColor := clDeepSkyBlue;
  Chart1.BottomAxis.TickColor := clWhite;
  Chart1.BottomAxis.Marks.LabelFont.Color := clWhite;
  Chart1.BottomAxis.Marks.LabelFont.Style := [fsBold];
  Chart1.LeftAxis.TickColor := clWhite;
  Chart1.LeftAxis.Marks.LabelFont.Color := clWhite;
  Chart1.LeftAxis.Marks.LabelFont.Style := [fsBold];
end;

procedure TForm1.Panel1Paint(Sender: TObject);
begin
  Panel1.Canvas.GradientFill(Rect(0, 0, Panel1.Width, Chart1.Top),
                             clSkyBlue, clDeepSkyBlue, gdVertical);
end;

end.

