unit unitWeatherApi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  unitWeatherData,
  fpjson,
  jsonparser;


type

  { TWeatherApiApi }

  TWeatherApiApi = class
  private
    function ExtractWeatherDataFromStream(const AStream: TStream;
      ASubTitle: string; AForecastDays: integer = 0): TWeatherOutlook;
    procedure LoadCurrentFromJson(AjObject: TJSONObject;
      ACurrentData: TCurrentData);
    procedure LoadForecastFromJson(AjObject: TJSONObject;
      AForecastDays: integer; AForecasts: TForecastDataList);
    procedure LogJsonResponse(AjObject: TJSONObject; ASubTitle: string);
  public
    function GetCurrentWeather(ALocation: string): TWeatherOutlook;
    function GetWeatherForecast(ALocation: string; ADays: integer): TWeatherOutlook;
  end;

implementation

uses
  LazLogger,
  unitApiKey,
  unitHttpClient;

const
  WeatherApiUrl = 'https://api.weatherapi.com/v1/';
  CurrentWeatherUrl = WeatherApiUrl + 'current.json?';
  ForecastWeatherUrl = WeatherApiUrl + 'forecast.json?';

{ TWeatherApiApi }

procedure TWeatherApiApi.LoadCurrentFromJson(AjObject : TJSONObject;
                                             ACurrentData: TCurrentData);
var
  LIcon: TJSONStringType;
begin
  ACurrentData.Location := AjObject.FindPath('location.name').AsString;
  ACurrentData.CurrentTempC := AjObject.FindPath('current.temp_c').AsFloat;
  ACurrentData.ConditionText := AjObject.FindPath('current.condition.text').AsString;
  LIcon := AjObject.FindPath('current.condition.icon').AsString;
  if LIcon <> '' then
    ACurrentData.WeatherImage := HttpGetRequest(LIcon);
end;

procedure TWeatherApiApi.LoadForecastFromJson(AjObject : TJSONObject;
                                              AForecastDays: integer;
                                              AForecasts: TForecastDataList);
var
  LIcon: TJSONStringType;
  LTemp: TForecastData;
  I: Integer;
  jItems: TJSONData;
  jItem: TJSONObject;
begin
  jItems := AjObject.FindPath('forecast.forecastday');
  for I := 0 to jItems.Count-1 do
  begin
    jItem := jItems.Items[I] as TJSONObject;
    LTemp := TForecastData.Create;
    LTemp.MaxTempC := jItem.FindPath('day.maxtemp_c').AsFloat;
    LTemp.MinTempC := jItem.FindPath('day.mintemp_c').AsFloat;
    LTemp.AvgTempC := jItem.FindPath('day.avgtemp_c').AsFloat;
    LTemp.TotalPrecipMM := jItem.FindPath('day.totalprecip_mm').AsFloat;
    LTemp.WillItRain := jItem.FindPath('day.daily_will_it_raim').AsInteger = 1;
    LTemp.ChanceofRain := jItem.FindPath('day.daily_chance_of_raim').AsInteger;
    LTemp.ConditionText := jItem.FindPath('day.condition.text').AsString;
    LIcon := AjObject.FindPath('day.condition.icon').AsString;
    if LIcon <> '' then
      LTemp.WeatherImage := HttpGetRequest(LIcon);
    AForecasts.Add(LTemp);
  end;
end;

procedure TWeatherApiApi.LogJsonResponse(AjObject: TJSONObject; ASubTitle: string);
const
  LogHeader = '--- Response from WeatherApi ---------------------------------';
  LogFooter = '==============================================================';
begin
  DebugLogger.DebugLn(LogHeader + sLineBreak +
                      ASubTitle + ':' + sLineBreak +
                      AjObject.FormatJSON() + sLineBreak +
                      LogFooter);
end;

function TWeatherApiApi.ExtractWeatherDataFromStream(const AStream: TStream;
  ASubTitle: string; AForecastDays: integer): TWeatherOutlook;
var
  jData, jForecastDay: TJSONData;
  jObject : TJSONObject;
  weather : TWeatherOutlook;
begin
  Result := nil;
  jData := GetJSON(AStream);
  try
    jObject := jData as TJSONObject;
    weather := TWeatherOutlook.Create;
    try
      LogJsonResponse(jObject, ASubTitle);
      LoadCurrentFromJson(jObject, weather.Current);
      if AForecastDays > 0 then
        LoadForecastFromJson(jObject,
                             AForecastDays,
                             weather.Forecast);
      Result := weather;
    except
      on e: Exception do
      begin
        DebugLogger.DebugLn('exception parsing json response - ' + e.Message);
        {weather.Free}
        raise;
      end;
    end;
  finally
    jData.Free;
  end;
end;

function TWeatherApiApi.GetCurrentWeather(ALocation: string): TWeatherOutlook;
var
  URL: String;
  LResponse: TStringStream;
begin
  URL := CurrentWeatherUrl +
    'key=' + WeatherApiKey +
    '&q=' + ALocation + '&aqi=no';

  LResponse := HttpGetRequest(URL);
  try
    Result := ExtractWeatherDataFromStream(LResponse, 'Current Weather Report');
  finally
    LResponse.Free;
  end;
end;

function TWeatherApiApi.GetWeatherForecast(ALocation: string; ADays: integer
  ): TWeatherOutlook;
var
  URL: String;
  LResponse: TStringStream;
begin
  URL := ForecastWeatherUrl +
    'key=' + WeatherApiKey +
    '&q=' + ALocation +
    '&days=' + ADays.ToString +
    '&aqi=no&alerts=no';

  DebugLogger.DebugLn('TWeatherApiApi.GetWeatherForecast - ' + URL);
  LResponse := HttpGetRequest(URL);
  try
    try
      Result := ExtractWeatherDataFromStream(LResponse,
                                             'Forecast Weater Report',
                                             ADays);

    except
      on e: exception do
      begin
        DebugLogger.DebugLn('exception in GetWeatherForecast - ' + e.Message +
          'URL=' + URL);
      end;
    end;
  finally
    LResponse.Free;
  end;
end;

end.

