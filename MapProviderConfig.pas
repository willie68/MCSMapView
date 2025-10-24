unit MapProviderConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  // display type for the layer
  TDisplayType = (dtAlpha, dtOpaque, dtForeground);

  { TMapProviderConfig
    Represents the configuration of a map provider.
    JSON field names (english, lowercase): name, description, url, display_type, projection, min_zoom, max_zoom
  }
  TMapProviderConfig = class
  private
    FName: string;
    FDescription: string;
    FURL: string;
    FDisplayType: TDisplayType;
    FProjection: string;
    FMinZoom: Integer;
    FMaxZoom: Integer;
  public
    constructor Create;
    // JSON conversion
    function ToJSONObject: TJSONObject;
    function ToJSONString: string;
    class function FromJSONObject(AObj: TJSONObject): TMapProviderConfig;
    class function FromJSONString(const AStr: string): TMapProviderConfig;

    // Properties
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property URL: string read FURL write FURL;
    property DisplayType: TDisplayType read FDisplayType write FDisplayType;
    property Projection: string read FProjection write FProjection;
    property MinZoom: Integer read FMinZoom write FMinZoom;
    property MaxZoom: Integer read FMaxZoom write FMaxZoom;
  end;

// Helper functions for enum conversion (use lowercase JSON values)
function DisplayTypeToString(AType: TDisplayType): string;
function DisplayTypeFromString(const S: string): TDisplayType;

implementation

{ Helper functions }

function DisplayTypeToString(AType: TDisplayType): string;
begin
  case AType of
    dtAlpha: Result := 'alpha';
    dtOpaque: Result := 'opaque';
    dtForeground: Result := 'foreground';
  else
    Result := 'opaque';
  end;
end;

function DisplayTypeFromString(const S: string): TDisplayType;
var
  t: string;
begin
  t := Trim(S);
  if SameText(t, 'alpha') then Exit(dtAlpha);
  if SameText(t, 'foreground') then Exit(dtForeground);
  if SameText(t, 'opaque') then Exit(dtOpaque);
  // Also accept capitalized variants for robustness
  if SameText(t, 'Alpha') then Exit(dtAlpha);
  if SameText(t, 'Foreground') then Exit(dtForeground);
  if SameText(t, 'Opaque') then Exit(dtOpaque);
  // Default
  Result := dtOpaque;
end;

{ TMapProviderConfig }

constructor TMapProviderConfig.Create;
begin
  inherited Create;
  FName := '';
  FDescription := '';
  FURL := '';
  FDisplayType := dtOpaque;
  FProjection := 'EPSG:3857';
  FMinZoom := 0;
  FMaxZoom := 22;
end;

function TMapProviderConfig.ToJSONObject: TJSONObject;
begin
  // Caller is responsible for freeing the returned TJSONObject.
  Result := TJSONObject.Create;
  Result.Add('name', FName);
  Result.Add('description', FDescription);
  Result.Add('url', FURL);
  Result.Add('display_type', DisplayTypeToString(FDisplayType));
  Result.Add('projection', FProjection);
  Result.Add('min_zoom', FMinZoom);
  Result.Add('max_zoom', FMaxZoom);
end;

function TMapProviderConfig.ToJSONString: string;
var
  obj: TJSONObject;
begin
  obj := ToJSONObject;
  try
    Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

class function TMapProviderConfig.FromJSONObject(AObj: TJSONObject): TMapProviderConfig;
var
  cfg: TMapProviderConfig;
  v: TJSONData;
begin
  if AObj = nil then
    raise Exception.Create('FromJSONObject: AObj = nil');

  cfg := TMapProviderConfig.Create;
  try
    // name
    v := AObj.Find('name');
    if Assigned(v) then cfg.FName := v.AsString;

    // description
    v := AObj.Find('description');
    if Assigned(v) then cfg.FDescription := v.AsString;

    // url
    v := AObj.Find('url');
    if Assigned(v) then cfg.FURL := v.AsString;

    // display_type
    v := AObj.Find('display_type');
    if Assigned(v) then cfg.FDisplayType := DisplayTypeFromString(v.AsString);

    // projection
    v := AObj.Find('projection');
    if Assigned(v) then cfg.FProjection := v.AsString;

    // min_zoom
    v := AObj.Find('min_zoom');
    if Assigned(v) then cfg.FMinZoom := v.AsInteger;

    // max_zoom
    v := AObj.Find('max_zoom');
    if Assigned(v) then cfg.FMaxZoom := v.AsInteger;

    // Sanity check: if min > max, swap
    if cfg.FMinZoom > cfg.FMaxZoom then
    begin
      var tmp := cfg.FMinZoom;
      cfg.FMinZoom := cfg.FMaxZoom;
      cfg.FMaxZoom := tmp;
    end;

    Result := cfg;
  except
    cfg.Free;
    raise;
  end;
end;

class function TMapProviderConfig.FromJSONString(const AStr: string): TMapProviderConfig;
var
  jsonData: TJSONData;
  obj: TJSONObject;
begin
  jsonData := GetJSON(AStr);
  try
    if not (jsonData is TJSONObject) then
      raise Exception.Create('FromJSONString: JSON is not an object');
    obj := TJSONObject(jsonData);
    Result := FromJSONObject(obj);
  finally
    jsonData.Free;
  end;
end.
