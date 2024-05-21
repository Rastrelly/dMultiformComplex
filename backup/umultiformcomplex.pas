unit uMultiformComplex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  uMultiformComplexTools, DateUtils;

type

  { TForm1 }

  TSnowMaker = class
    public
      isActive:boolean;
      cols, colH:integer;
      colContents:array of integer;
      constructor Create(nCols:integer; colHeight:integer);
  end;

  PSnowMaker = ^TSnowMaker;

  TSnowFlake = class
  private
    x,y:real;
    speed:real;
    snowMakerRef:PSnowMaker;
  public
    finished:boolean;
    procedure Move(dt:real);
    constructor Create(sMaker:PSnowMaker);
  end;

  TSnowFlakeController = class
    private
      sFlakeAttempts:integer;
      snowMakerRef:PSnowMaker;
    public
      maxSFlakes:integer;
      sFlakes:array of TSnowFlake;
      procedure SpawnSFlake;
      procedure ManageSFlakes(mSeconds:integer);
      constructor Create(mSFlakes:integer;sMaker:PSnowMaker);
  end;

  TRenderer = class(TThread)
    public
      procedure runSnowflakeProcess;
      procedure Execute; override;
      Constructor Create(CreateSuspended : boolean);
  end;


  TForm1 = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure RenderSnow;
  private

  public

  end;

var
  Form1: TForm1;
  Form2: TForm2;

  SMaker:TSnowMaker;
  SController:TSnowFlakeController;
  SRenderer:TRenderer;

  tsNow, tsLast:TDateTime;

  needQuit: boolean=false;



implementation

{$R *.lfm}

{ TForm1 }



constructor TSnowMaker.Create(nCols:integer; colHeight:integer);
begin
  isActive:=false;
  cols:=nCols;
  colH:=colHeight;
  SetLength(colContents,cols);
end;

constructor TSnowFlake.Create(sMaker:PSnowMaker);
begin
  snowMakerRef:=sMaker;
  finished:=false;
  x:=Random(snowMakerRef^.cols);
  y:=0;
  speed:=Random(391)+100;
  Form2.Log('I am spawned at '+floattostr(x)+'; '+FloatToStr(y)+'; spd = '+FloatToStr(speed));
end;

procedure TSnowFlake.Move(dt:real);
begin
  y:=y+dt*speed;
  if (y>snowMakerRef^.colH-snowMakerRef^.colContents[round(x)]) then
  begin
    finished:=true;
    inc(snowMakerRef^.colContents[round(x)]);
    Form2.Log('Removing a snowflake at '+IntToStr(round(x))+', '+inttostr(Round(y)));
  end;
end;

procedure TSnowFlakeController.SpawnSFlake;
var i:integer;
begin
  i:=0;
  while (sFlakes[i]<>Nil) do
  begin
    inc(i);
    if (i>=maxSFlakes) then Break;
  end;

  if (i<maxSFlakes) then
  begin
    sFlakes[i]:=TSnowFlake.Create(snowMakerRef);
    Form2.Log('snowflake spawned at id = '+inttostr(i));
  end
  else
  begin
    Form2.Log('Could not find a place to spawn a snowflake at id = '+inttostr(i)+'!');
  end;
end;

procedure TSnowFlakeController.ManageSFlakes(mSeconds:integer);
var i,j,chance:integer;
begin
  chance:=Random(101)+mSeconds;

  //runs snowflakes
  for i:=0 to maxSFlakes-1 do
  begin
    if (sFlakes[i]<>Nil) then
    begin
      sFlakes[i].Move(mSeconds/1000);
      if sFlakes[i].finished then FreeAndNil(sFlakes[i]);
    end;
  end;

  //tries to spawn a new snowflake
  for j:=0 to (Form2.snowfallIntensity-1) do
  begin
    if ((chance+sFlakeAttempts)>80) then
    begin
      sFlakeAttempts:=0;
      SpawnSFlake;
    end
    else
    begin
      Inc(sFlakeAttempts);
    end;
  end;

end;

constructor TSnowFlakeController.Create(mSFlakes:integer;sMaker:PSnowMaker);
var i:integer;
begin
  snowMakerRef:=sMaker;
  maxSFlakes:=mSFlakes;
  SetLength(sFlakes,mSFlakes);
  for i:=0 to maxSFlakes-1 do
  begin
    try
      FreeAndNil(sFlakes[i]);
    finally
      sFlakes[i]:=nil;
    end;
  end;
  sFlakeAttempts:=0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  Form2:=TForm2.Create(Form1);
  Form2.Show;
  SMaker:=TSnowMaker.Create(640,480);
  SController:=TSnowFlakeController.Create(10000,@SMaker);
  Form2.TrackBar1.Position:=Timer1.Interval;
  tsNow:=now; tsLast:=now;
  SRenderer:=TRenderer.Create(false);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  needQuit:=true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var tid:integer;
begin
  Timer1.Enabled:=false;
end;



procedure TForm1.RenderSnow;
var i,j,rw,rh:integer;
    render:TBitmap;
begin
  Image1.Picture.Bitmap.Width:=Image1.Width;
  Image1.Picture.Bitmap.Height:=Image1.Height;
  render:=TBitmap.Create;
  rw:=SMaker.cols;
  rh:=SMaker.colH;
  render.Width:=rw;
  render.Height:=rh;

  with render.Canvas do
  begin
    //1 - background
    Pen.Color:=Form2.BgColor;
    Brush.Color:=Form2.BgColor;
    Rectangle(0,0,rw,rh);

    Pixels[round(rw/2),round(rh/2)]:=clRed;

    //2 - bottom snow
    for i:=0 to rw-1 do
    if (SMaker.colContents[i]>0) then
    for j:=0 to SMaker.colContents[i]-1 do
    begin
       Pixels[i,rh-j-1]:=Form2.SnowColor;
    end;

    //3 - falling snow
    if SMaker.isActive then
    for i:=0 to SController.maxSFlakes-1 do
    begin
      if (SController.sFlakes[i] <> Nil) then
      begin
        Pixels[round(SController.sFlakes[i].x),round(SController.sFlakes[i].y)]:=Form2.SnowColor;
      end;
    end;
  end;

  Image1.Canvas.CopyRect(Rect(0,0,Image1.Width,Image1.Height),render.Canvas,
                         Rect(0,0,rw,rh));

  render.Free;
end;

procedure TRenderer.Execute;
begin
  while not needQuit do
  begin
    runSnowflakeProcess;
    Synchronize(@(Form1.RenderSnow));
  end;
end;

procedure TRenderer.runSnowflakeProcess;
var deltaMS:integer;
begin
    tsNow:=Now;
    deltaMS:=MilliSecondsBetween(tsNow,tsLast);
    tsLast:=tsNow;

    SMaker.isActive:=Form2.activeState;
    if SMaker.isActive then
    begin
      SController.ManageSFlakes(deltaMS);
    end;

end;

Constructor TRenderer.Create(CreateSuspended : boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate:=true;
end;

end.

