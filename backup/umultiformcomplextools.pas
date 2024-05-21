unit uMultiformComplexTools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls;

type

  { TForm2 }
  PColorDialog = ^TColorDialog;
  PColor = ^TColor;

  TForm2 = class(TForm)
    btnStart: TButton;
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    cdSnow: TColorDialog;
    cdBG: TColorDialog;
    edtSnowIntensity: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    TrackBar1: TTrackBar;
    udSnowfallIntensity: TUpDown;
    procedure btnStartClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure edtSnowIntensityChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure udSnowfallIntensityChanging(Sender: TObject;
      var AllowChange: Boolean);
  private

  public
    activeState:boolean;
    snowfallIntensity:integer;
    timerInterval:integer;
    SnowColor,BgColor:TColor;
    procedure Log(txt:string);
    procedure UpdateSFIntensity;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  activeState:=false;
  UpdateSFIntensity;
  SnowColor:=clWhite;
  BgColor:=clSkyBlue;
end;

procedure TForm2.TrackBar1Change(Sender: TObject);
begin
  timerInterval:=TrackBar1.Position;
end;

procedure TForm2.udSnowfallIntensityChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  edtSnowIntensity.Text:=IntToStr(udSnowfallIntensity.Position);
end;

procedure TForm2.btnStartClick(Sender: TObject);
begin
  if (activeState=false) then
  begin
    activeState:=true;
    btnStart.Caption:='Stop';
  end
  else
  begin
    activeState:=false;
    btnStart.Caption:='Start';
  end;
end;

procedure SwapColorByDialog(dial:PColorDialog; clr:PColor);
begin
  dial^.Color:=clr^;
  if (dial^.Execute) then
  begin
    clr^:=dial^.Color;
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  SwapColorByDialog(@cdSnow, @SnowColor);
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  SwapColorByDialog(@cdBG, @BgColor);
end;

procedure TForm2.edtSnowIntensityChange(Sender: TObject);
begin
  UpdateSFIntensity;
end;

procedure TForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TForm2.Log(txt:string);
begin
  If CheckBox1.Checked then
  Memo1.Lines.Add(txt);
end;

procedure TForm2.UpdateSFIntensity;
begin
 if not TryStrToInt(edtSnowIntensity.Text,snowfallIntensity) then
 begin
   snowfallIntensity:=1;
   udSnowfallIntensity.Position:=snowfallIntensity;
 end
 else
 begin
   udSnowfallIntensity.Position:=snowfallIntensity;
 end;

end;

end.

