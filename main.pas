unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons,
  StdCtrls, JSONPropStorage, ExtCtrls, ComboEx, mvMapViewer, mvTypes,
  mvMapProvider, mvPluginCommon, mvPlugins;

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
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
  private
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
    if LazStartsStr('OpenStreet', mapName) then
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
    GermanyCenter();
  end;
end;

procedure Tfrmmain.FormCreate(Sender: TObject);
var
  provider: TStringList;
  i: integer;
  mapname: string;
begin
  MapView1.Align := alClient;
  PopulateLayers();

  provider := TStringList.Create();
  MapProvidersToSortedStrings(provider);
  cbProvider.Items.Clear();
  for i := 0 to Provider.Count - 1 do
  begin
    mapName := Provider.Strings[i];
    if LazStartsStr('OpenStreet', mapName) or LazStartsStr('Google Maps', mapName) then
      cbProvider.Items.Add(mapName);
  end;
  cbProvider.ItemIndex := 0;
  MapView1.MapProvider := cbprovider.Items[0];
  MapView1.Active := True;
  Provider.Free();

  GermanyCenter();
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
