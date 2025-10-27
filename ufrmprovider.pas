unit ufrmProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, StdCtrls, Spin;

type

  { TfrmProvider }

  TfrmProvider = class(TForm)
    BitBtn1: TBitBtn;
    btnOK: TBitBtn;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    ledURL: TLabeledEdit;
    ledName: TLabeledEdit;
    ListBox1: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    rbOpaque: TRadioButton;
    rbAlpha: TRadioButton;
    rbDraw: TRadioButton;
    speZoomMin: TSpinEdit;
    speZoomMax: TSpinEdit;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
  private

  public

  end;

var
  frmProvider: TfrmProvider;

implementation

{$R *.lfm}

end.

