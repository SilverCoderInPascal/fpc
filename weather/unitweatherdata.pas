unit unitWeatherData;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Contnrs;

type

  { TForecastData }

  TForecastData = class
  public
    MinTempC: double;
    MaxTempC: double;
    AvgTempC: double;
    TotalPrecipMM: double;
    WillItRain: boolean;
    ChanceofRain: integer;
    ConditionText: string;
    WeatherImage: TStringStream;

    constructor Create;
    destructor Destroy; override;
  end;

  { TCurrentData }

  TCurrentData = class
  public
    Location: string;
    ConditionText: string;
    CurrentTempC: double;
    WeatherImage: TStringStream;

    constructor Create;
    destructor Destroy; override;
  end;

  TForecastDataList = class(TFPObjectList);

  { TWeatherOutlook }

  TWeatherOutlook = class
  public
    Current: TCurrentData;
    Forecast: TForecastDataList;

    constructor Create;
    destructor Destroy; override;
  end;

function DegCToStr(ADeg: double): string;
function FormatDegreesCelsius(temperature: double): string;

implementation


function FormatDegreesCelsius(temperature: Double): string;
var
  degreesSymbol: Char;
begin
  degreesSymbol := Chr($B0);
  Result := Format('%.1f%cC', [temperature, degreesSymbol]);
end;

function DegCToStr(ADeg: double): string;
begin
  Result := FormatFloat('0.0', ADeg) + '°C';
end;

{ TForecastData }

constructor TForecastData.Create;
begin
  MinTempC := 0.0;
  MaxTempC := 0.0;
  AvgTempC := 0.0;
  TotalPrecipMM := 0;
  WillItRain := False;
  ChanceofRain := 0;
  ConditionText := '';
  WeatherImage := nil;
end;

destructor TForecastData.Destroy;
begin
  if Assigned(WeatherImage) then
    WeatherImage.Free;
  WeatherImage := nil;
  inherited Destroy;
end;

{ TWeatherData }

constructor TCurrentData.Create;
begin
  Location := '';
  ConditionText := '';
  CurrentTempC := 0.0;
  WeatherImage := nil;
end;

destructor TCurrentData.Destroy;
begin
  if Assigned(WeatherImage) then
    WeatherImage.Free;
  WeatherImage := nil;
  inherited Destroy;
end;

{ TWeatherOutlook }

constructor TWeatherOutlook.Create;
begin
  Current := TCurrentData.Create;
  Forecast := TForecastDataList.Create;
end;

destructor TWeatherOutlook.Destroy;
begin
  Current.Free;
  Forecast.Free;
  inherited Destroy;
end;

end.

