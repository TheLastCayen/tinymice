{
######################################################
#         Copyright 2020 Daniel Babin                #
######################################################

This file is part of TinyMice.

TinyMice is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

TinyMice is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with TinyMice.  If not, see <https://www.gnu.org/licenses/>.es/>.
}

{
Uses uAbout;
.
.
.
Function....
Var
  VAbout:TFAbout;
begin
   VAbout:=TFAbout.Create(nil);
   VAbout.PageControl1.ActivePageIndex := 0;
  try
    VAbout.ShowModal;
  finally
    VAbout.Free;
  end;
End;

}
unit uabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, LCLIntF, ComCtrls, Process, lcltranslator;

type

  { TFAbout }

  TFAbout = class(TForm)
    Image1: TImage;
    LCopyright: TLabel;
    LEmail: TLabel;
    LImage: TLabel;
    LURL: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    PageControl1: TPageControl;
    TSAbout: TTabSheet;
    TSContributor: TTabSheet;
    TSChangelog: TTabSheet;
    procedure Label2Click(Sender: TObject);
    procedure LEmailClick(Sender: TObject);
    procedure LImageClick(Sender: TObject);
    procedure LURLClick(Sender: TObject);
  private
    { private declarations }

  public
    { public declarations }
  end;


var
  FAbout: TFAbout;

implementation

{$R *.lfm}

{ TFAbout }


procedure TFAbout.Label2Click(Sender: TObject);
begin

end;

procedure TFAbout.LEmailClick(Sender: TObject);
begin
  OpenURL('mailto:tinymice.github@gmail.com');
end;

procedure TFAbout.LImageClick(Sender: TObject);
begin
  OpenURL('http://www.iconarchive.com');
end;

procedure TFAbout.LURLClick(Sender: TObject);
begin
  OpenURL('https://github.com/TheLastCayen/tinymice')
end;



end.

