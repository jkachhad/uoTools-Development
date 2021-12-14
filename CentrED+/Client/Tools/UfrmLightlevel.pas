unit UfrmLightlevel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, UfrmToolWindow;

type

  { TfrmLightlevel }

  TfrmLightlevel = class(TfrmToolWindow)
    tbLightlevel: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure tbLightlevelChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmLightlevel: TfrmLightlevel;

implementation

uses
  UfrmMain, Language;

{ TfrmLightlevel }

procedure TfrmLightlevel.tbLightlevelChange(Sender: TObject);
begin
  if frmMain.LightManager.LightLevel = 0 then
  begin
    frmMain.LightManager.LightLevel := tbLightlevel.Position;
    frmMain.InvalidateFilter;
  end else
    frmMain.LightManager.LightLevel := tbLightlevel.Position;

  frmMain.acLightlevel.Tag:=frmMain.LightManager.LightLevel;
  frmMain.oglGameWindow.Repaint;
end;

procedure TfrmLightlevel.FormCreate(Sender: TObject);
begin
  LanguageTranslate(Self);
end;

initialization
  {$I UfrmLightlevel.lrs}

end.

