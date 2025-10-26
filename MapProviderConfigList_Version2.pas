unit MapProviderConfigList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, JSONPropStorage, MapProviderConfig;

type
  { TMapProviderConfigList
    Manages a list of map provider configurations with JSON storage support
  }
  TMapProviderConfigList = class
  private
    FList: TList;
    FStorage: TJSONPropStorage;
    function GetCount: Integer;
    function GetItem(Index: Integer): TMapProviderConfig;
  public
    constructor Create(AStorage: TJSONPropStorage);
    destructor Destroy; override;
    
    procedure LoadFromStorage;
    procedure SaveToStorage;
    function Add(AConfig: TMapProviderConfig): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMapProviderConfig read GetItem; default;
  end;

implementation

{ TMapProviderConfigList }

constructor TMapProviderConfigList.Create(AStorage: TJSONPropStorage);
begin
  inherited Create;
  FList := TList.Create;
  FStorage := AStorage;
  if Assigned(FStorage) then
    LoadFromStorage;
end;

destructor TMapProviderConfigList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TMapProviderConfigList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMapProviderConfigList.GetItem(Index: Integer): TMapProviderConfig;
begin
  Result := TMapProviderConfig(FList[Index]);
end;

procedure TMapProviderConfigList.LoadFromStorage;
var
  jsonStr: string;
  jsonData: TJSONData;
  jsonArray: TJSONArray;
  i: Integer;
  cfg: TMapProviderConfig;
begin
  Clear;
  
  if not Assigned(FStorage) then
    Exit;
    
  // Load JSON string from storage
  jsonStr := FStorage.ReadString('MapProviderConfigs', '[]');
  
  try
    jsonData := GetJSON(jsonStr);
    try
      if jsonData is TJSONArray then
      begin
        jsonArray := TJSONArray(jsonData);
        for i := 0 to jsonArray.Count - 1 do
        begin
          if jsonArray[i] is TJSONObject then
          begin
            cfg := TMapProviderConfig.FromJSONObject(TJSONObject(jsonArray[i]));
            FList.Add(cfg);
          end;
        end;
      end;
    finally
      jsonData.Free;
    end;
  except
    on E: Exception do
    begin
      // Log error or handle it appropriately
      Clear; // Reset to empty list on error
    end;
  end;
end;

procedure TMapProviderConfigList.SaveToStorage;
var
  jsonArray: TJSONArray;
  i: Integer;
  cfg: TMapProviderConfig;
begin
  if not Assigned(FStorage) then
    Exit;
    
  jsonArray := TJSONArray.Create;
  try
    for i := 0 to FList.Count - 1 do
    begin
      cfg := TMapProviderConfig(FList[i]);
      jsonArray.Add(cfg.ToJSONObject);
    end;
    
    FStorage.WriteString('MapProviderConfigs', jsonArray.AsJSON);
  finally
    jsonArray.Free;
  end;
end;

function TMapProviderConfigList.Add(AConfig: TMapProviderConfig): Integer;
begin
  Result := FList.Add(AConfig);
end;

procedure TMapProviderConfigList.Clear;
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
    TMapProviderConfig(FList[i]).Free;
  FList.Clear;
end;

procedure TMapProviderConfigList.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    TMapProviderConfig(FList[Index]).Free;
    FList.Delete(Index);
  end;
end;

end.