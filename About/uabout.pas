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
  StdCtrls, ExtCtrls, LCLIntF, ComCtrls, Process, DefaultTranslator;

type

  { TFAbout }

  TFAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure Label2Click(Sender: TObject);
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
  OpenURL('http://www.iconarchive.com');
end;



end.

