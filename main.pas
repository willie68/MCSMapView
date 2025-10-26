unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons,
  StdCtrls, JSONPropStorage, ExtCtrls, ComboEx, mvMapViewer, mvTypes,
  mvMapProvider, mvPluginCommon, mvPlugins, uMapProviderConfigList, uMapProviderConfig;

type

  { Tfrmmain }

  Tfrmmain = class(TForm)
    cbProvider: TComboBox;
    ccbOverlays: TCheckComboBox;
    ImageList1: TImageList;
    JSONPropStorage1: TJSONPropStorage;
    MapView1: TMapView;
    MvPluginManager1: TMvPluginManager;
    MvPluginManager1LegalNoticePlugin1: TLegalNoticePlugin;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure cbProviderChange(Sender: TObject);
    procedure ccbOverlaysItemChange(Sender: TObject; AIndex: integer);
    procedure CoolBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JSONPropStorage1RestoreProperties(Sender: TObject);
    procedure MapView1ZoomChange(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
  private
    FProviderConfigs: TMapProviderConfigList;

    FDepthlayer: TMapLayer;
    FSeamarkslayer: TMapLayer;
    FHabourlayer: TMapLayer;
    FTrackLayer: TMapLayer;

    procedure GermanyCenter();
    procedure PopulateLayers();
    procedure SetMapProvider();
  public

  end;

var
  frmmain: Tfrmmain;

implementation

uses mvDrawingEngine, LazStringUtils;
  {$R *.lfm}

  { Tfrmmain }

procedure Tfrmmain.CoolBar1Change(Sender: TObject);
begin

end;

procedure Tfrmmain.cbProviderChange(Sender: TObject);
begin
  SetMapProvider();
end;

procedure Tfrmmain.ccbOverlaysItemChange(Sender: TObject; AIndex: integer);
var
  i: integer;
begin
  i := AIndex;
  if ccbOverlays.Items[i] = 'OpenSeaMap Seamarks' then
  begin
    FSeamarkslayer.Visible := ccbOverlays.Checked[i];
  end;
  if ccbOverlays.Items[i] = 'OpenSeaMap Habour' then
  begin
    FHabourlayer.Visible := ccbOverlays.Checked[i];
  end;
  if ccbOverlays.Items[i] = 'OpenSeaMap Gebco' then
  begin
    FDepthlayer.Visible := ccbOverlays.Checked[i];
  end;
end;

procedure TfrmMain.SetMapProvider();
var
  mapName: string;
  legal: string;
begin
  MapView1.Active := False;
  mapName := cbProvider.Text;
  MapView1.MapProvider := mapName;
  if not (mapName = '') then
  begin
    legal := '';
    if LazStartsStr('OpenStreet', mapName) OR LazStartsStr('OpenSea', mapName) then
    begin
      legal :=
        'Map data from [https://www.openstreetmap.org/copyright OpenStreetMap and contributors]';
    end;
    if LazStartsStr('Google Maps', mapName) then
    begin
      legal := 'Map data from [https://www.google.com/help/legalnotices_maps/ Google Maps]';
    end;
    MvPluginManager1LegalNoticePlugin1.LegalNotice := legal;
    MapView1.Active := True;
    //GermanyCenter();
  end;
end;

procedure Tfrmmain.FormCreate(Sender: TObject);
var
  provider: TStringList;
  i: integer;
  mapname: string;
  cfg : TMapProviderConfig;
begin
  MapView1.Align := alClient;
  PopulateLayers();

  FProviderConfigs := TMapProviderConfigList.Create(JSONPropStorage1);

  // Optional: Standardkonfigurationen hinzufügen, falls Liste leer
  if FProviderConfigs.Count = 0 then
  begin
    cfg := TMapProviderConfig.Create;
    cfg.Name := 'OpenStreetMap';
    cfg.Description := 'Standard OpenStreetMap tiles';
    cfg.URL := 'https://tile.openstreetmap.org/{z}/{x}/{y}.png';
    cfg.DisplayType := dtOpaque;
    FProviderConfigs.Add(cfg);

    // Speichern der Standardkonfiguration
    FProviderConfigs.SaveToStorage;
  end;

  provider := TStringList.Create();
  MapProvidersToSortedStrings(provider);
  cbProvider.Items.Clear();
  for i := 0 to Provider.Count - 1 do
  begin
    mapName := Provider.Strings[i];
    if LazStartsStr('OpenStreet', mapName) or LazStartsStr('OpenSeaMap Adria', mapName) or LazStartsStr('Google Maps', mapName) then
      cbProvider.Items.Add(mapName);
  end;
  cbProvider.ItemIndex := 0;
  MapView1.MapProvider := cbprovider.Items[0];
  MapView1.Active := True;
  Provider.Free();

  GermanyCenter();
end;

procedure Tfrmmain.FormDestroy(Sender: TObject);
begin
   FProviderConfigs.Free;
end;

procedure Tfrmmain.JSONPropStorage1RestoreProperties(Sender: TObject);
begin

end;

procedure Tfrmmain.MapView1ZoomChange(Sender: TObject);
begin
  StatusBar1.Panels[2].Text:= IntToStr(MapView1.Zoom);
end;

procedure Tfrmmain.ToolButton1Click(Sender: TObject);
begin
  Close();
end;

procedure Tfrmmain.ToolButton3Click(Sender: TObject);
begin
  MapView1.Engine.ClearCache;
end;

procedure Tfrmmain.GermanyCenter();
var
  germany: TRealPoint;
begin
  germany.InitXY(10.0, 51.0);
  MapView1.Center := Germany;
  MapView1.Zoom := 6;
  MapView1.Redraw;
end;

procedure TfrmMain.PopulateLayers();
begin
  RegisterMapProvider('OpenSeaMap Seamarks', ptEPSG3857,
    'https://tiles.openseamap.org/seamark/%z%/%x%/%y%.png', 0, 19, 3, @GetSvrLetter);
  RegisterMapProvider('OpenSeaMap Habour', ptEPSG3857,
    'https://t1.openseamap.org/habour/%z%/%x%/%y%.png', 0, 19, 3, @GetSvrLetter);
  RegisterMapProvider('OpenSeaMap Gebco', ptEPSG3857,
    'http://localhost:8580/tileserver/gebco/xyz/%z%/%x%/%y%.png', 0,
    19, 3, @GetSvrLetter);
  RegisterMapProvider('OpenSeaMap Adria', ptEPSG3857,
    'http://localhost:8580/tileserver/adria/xyz/%z%/%x%/%y%.png', 0,
    19, 3, @GetSvrLetter);
  RegisterMapProvider('OpenStreetMap Germany', ptEPSG3857,
    'http://localhost:8580/tileserver/osmde/xyz/%z%/%x%/%y%.png', 0,
    19, 3, @GetSvrLetter);

  ccbOverlays.ITems.Clear;
  ccbOverlays.Items.Add('OpenSeaMap Seamarks');
  ccbOverlays.Items.Add('OpenSeaMap Habour');
  ccbOverlays.Items.Add('OpenSeaMap Gebco');

  FDepthlayer := MapView1.Layers.Add as TMapLayer;
  FDepthlayer.Visible := False;
  FDepthlayer.MapProvider := 'OpenSeaMap Gebco';
  FDepthlayer.DrawMode := idmUseOpacity;

  FSeamarkslayer := MapView1.Layers.Add as TMapLayer;
  FSeamarkslayer.Visible := False;
  FSeamarkslayer.MapProvider := 'OpenSeaMap Seamarks';
  FSeamarkslayer.DrawMode := idmUseSourceAlpha;

  FHabourlayer := MapView1.Layers.Add as TMapLayer;
  FHabourlayer.Visible := False;
  FHabourlayer.MapProvider := 'OpenSeaMap Habour';
  FHabourlayer.DrawMode := idmUseSourceAlpha;

  FTrackLayer := MapView1.Layers.Add as TMapLayer;
  FTrackLayer.Visible := False;
end;


end.
